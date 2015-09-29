(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)
open Result
open Error

module Make(Log: S.LOG)(FLOW: V1_LWT.FLOW) = struct
  open Log

  type fid = Types.Fid.t

  module TagSet = Set.Make(Types.Tag)
  module TagMap = Map.Make(Types.Tag)

  type t = {
    flow: FLOW.flow;
    root: fid;
    msize: int32;
    transmit_m: Lwt_mutex.t;
    mutable wakeners: Response.payload Lwt.u TagMap.t;
    mutable free_tags: TagSet.t;
    mutable input_buffer: Cstruct.t;
  }

  (* For converting flow errors *)
  let (>>|=) m f =
    let open Lwt in
    m >>= function
    | `Ok x -> f x
    | `Eof -> return (error_msg "Caught EOF on underlying FLOW")
    | `Error e -> return (error_msg "Unexpected error on underlying FLOW: %s" (FLOW.error_message e))

  (* For Result + Lwt *)
  let (>>*=) m f =
   let open Lwt in
   m >>= function
   | Ok x -> f x
   | Error x -> Lwt.return (Error x)

  let read_exactly t n =
    let output = Cstruct.create n in
    let rec fill tofill = match Cstruct.len tofill with
      | 0 -> Lwt.return (Ok ())
      | n ->
        ( if Cstruct.len t.input_buffer = 0
          then (FLOW.read t.flow >>|= fun b -> Lwt.return (Ok b))
          else Lwt.return (Ok t.input_buffer)
        ) >>*= fun input ->
        let avail = min n (Cstruct.len input) in
        Cstruct.blit input 0 tofill 0 avail;
        t.input_buffer <- Cstruct.shift input avail;
        fill (Cstruct.shift tofill avail) in
    fill output
    >>*= fun () ->
    Lwt.return (Ok output)

  let read_one_packet t =
    let length_buffer = Cstruct.create 4 in
    read_exactly t 4
    >>*= fun length_buffer ->
    let length = Cstruct.LE.get_uint32 length_buffer 0 in
    read_exactly t (Int32.to_int length - 4)
    >>*= fun packet_buffer ->
    (* XXX: remove this data copy *)
    let buffer = Cstruct.create (Cstruct.len length_buffer + (Cstruct.len packet_buffer)) in
    Cstruct.blit length_buffer 0 buffer 0 (Cstruct.len length_buffer);
    Cstruct.blit packet_buffer 0 buffer (Cstruct.len length_buffer) (Cstruct.len packet_buffer);
    Lwt.return (Ok buffer)
    >>*= fun buffer ->
    Lwt.return (Response.read buffer)
    >>*= fun (response, _) ->
    Lwt.return (Ok response)

  let write_one_packet flow request =
    let sizeof = Request.sizeof request in
    let buffer = Cstruct.create sizeof in
    Lwt.return (Request.write request buffer)
    >>*= fun _ ->
    FLOW.write flow buffer
    >>|= fun () ->
    Lwt.return (Ok ())

  let rpc t request =
    (* Allocate a fresh tag, or wait if none are available yet *)
    let c = Lwt_condition.create () in
    let rec allocate_tag () =
      let open Lwt in
      if t.free_tags = TagSet.empty
      then Lwt_condition.wait c >>= fun () -> allocate_tag ()
      else
        let tag = TagSet.min_elt t.free_tags in
        t.free_tags <- TagSet.remove tag t.free_tags;
        let th, wakener = Lwt.task () in
        t.wakeners <- TagMap.add tag wakener t.wakeners;
        return (tag, th) in
    let deallocate_tag tag =
      t.free_tags <- TagSet.add tag t.free_tags;
      t.wakeners <- TagMap.remove tag t.wakeners;
      Lwt_condition.signal c () in
    let with_tag f =
      let open Lwt in
      allocate_tag ()
      >>= fun (tag, th) ->
      Lwt.catch
        (fun () -> f (tag, th))
        (fun e -> deallocate_tag tag; fail e) in
    with_tag
      (fun (tag, th) ->
        (* Lock the flow for output and transmit the packet *)
        Lwt_mutex.with_lock t.transmit_m
          (fun () ->
            write_one_packet t.flow { Request.tag; payload = request }
          )
        >>*= fun () ->
        (* Wait for the response to be read *)
        let open Lwt in
        th >>= fun response ->
        return (Ok response)
      )

  (* The dispatcher thread reads responses from the FLOW and wakes up
     the thread blocked in the rpc function. *)
  let rec dispatcher_t t =
    read_one_packet t
    >>*= fun response ->
    let tag = response.Response.tag in
    if not(TagMap.mem tag t.wakeners) then begin
      error "Received response with unexpected tag: %s" (Sexplib.Sexp.to_string (Response.sexp_of_t response));
      dispatcher_t t
    end else begin
      let wakener = TagMap.find tag t.wakeners in
      Lwt.wakeup_later wakener response.Response.payload;
      dispatcher_t t
    end

  let return_error = function
    | Response.Err { Response.Err.ename } ->
      Lwt.return (Error (`Msg ename))
    | payload ->
      Lwt.return (error_msg "Server sent unexpected reply: %s" (Sexplib.Sexp.to_string (Response.sexp_of_payload  payload)))

  let walk t fid newfid wnames =
    rpc t Request.(Walk { Walk.fid; newfid; wnames })
    >>*= function
    | Response.Walk x -> Lwt.return (Ok x)
    | response -> return_error response

  let openfid t fid mode =
    rpc t Request.(Open { Open.fid; mode })
    >>*= function
    | Response.Open x -> Lwt.return (Ok x)
    | response -> return_error response

  let read t fid offset count =
    rpc t Request.(Read { Read.fid; offset; count })
    >>*= function
    | Response.Read x -> Lwt.return (Ok x)
    | response -> return_error response
 
  let readdir t path =
    let fid = t.root in
    let newfid = match Types.Fid.of_int32 4l with Ok x -> x | _ -> assert false in
    let wnames = path in
    walk t fid newfid wnames
    >>*= fun _ -> (* I don't need to know the qids *)
    openfid t newfid Types.Mode.Read
    >>*= fun _ ->
    read t newfid 0L 1024l
    >>*= fun { Response.Read.data } ->
    Lwt.return (Ok [])

  let connect flow ?(msize = 1024l) ?(username = "nobody") ?(aname = "/") () =
    write_one_packet flow {
      Request.tag = Types.Tag.notag;
      payload = Request.Version Request.Version.({ msize; version = Types.Version.default });
    } >>*= fun () ->

    (* We use the convention that fid 0l is the root fid. We'll never clunk
       this one so we can always re-explore the filesystem from the root. *)
    let root = match Types.Fid.of_int32 0l with Ok x -> x | _ -> assert false in
    let transmit_m = Lwt_mutex.create () in
    let wakeners = TagMap.empty in
    (* We will use up to 100 tags, to avoid overloading the server. We only
       need 1 tag per outstanding RPC *)
    let free_tags =
      let rec loop acc = function
        | 0 -> acc
        | next ->
          let tag = match Types.Tag.of_int next with Ok x -> x | _ -> assert false in
          loop (TagSet.add tag acc) (next - 1) in
      loop TagSet.empty 100 in
    let t = { flow; root; msize; transmit_m; wakeners; free_tags; input_buffer = Cstruct.create 0 } in

    read_one_packet t
    >>*= fun response ->
    match response with
    | { Response.payload = Response.Version { Response.Version.msize; version }} when version = Types.Version.default ->
      let msize = min t.msize msize in
      let t = { t with msize } in

      let tag = match Types.Tag.of_int 0 with Ok x -> x | _ -> assert false in
      let afid = Types.Fid.nofid in
      write_one_packet flow {
        Request.tag;
        payload = Request.Attach Request.Attach.({ fid = root; afid; uname = username; aname })
      } >>*= fun () ->
      read_one_packet t
      >>*= fun response ->
      begin match response with
      | { Response.payload = Response.Attach { Response.Attach.qid } } ->
        info "Successfully received a root qid: %s\n%!" (Sexplib.Sexp.to_string_hum (Types.Qid.sexp_of_t qid));
        (* Negotiation complete: start the dispatcher thread *)
        let t = { t with msize } in
        Lwt.async (fun () -> dispatcher_t t);
        Lwt.return (Ok t)
      | { Response.payload = Response.Err { Response.Err.ename } } ->
        Lwt.return (Error (`Msg ename))
      | _ ->
        Lwt.return (error_msg "Server sent unexpected attach reply: %s" (Response.to_string response))
      end
    | { Response.payload = Response.Err { Response.Err.ename } } ->
      Lwt.return (Error (`Msg ename))
    | _ ->
      Lwt.return (error_msg "Server sent unexpected version reply: %s" (Response.to_string response))
end
