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

  type t = {
    flow: FLOW.flow;
    root: fid;
    msize: int32;
    maximum_payload: int32;
    transmit_m: Lwt_mutex.t;
    mutable wakeners: Response.payload Lwt.u Types.Tag.Map.t;
    mutable free_tags: Types.Tag.Set.t;
    mutable free_fids: Types.Fid.Set.t;
    free_fids_c: unit Lwt_condition.t;
    mutable input_buffer: Cstruct.t;
  }

  type connection = t

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
    let pretty_printed = Sexplib.Sexp.to_string (Response.sexp_of_t response) in
    debug "<- %s" pretty_printed;
    Lwt.return (Ok response)

  let write_one_packet flow request =
    debug "-> %s" (Request.to_string request);
    let sizeof = Request.sizeof request in
    let buffer = Cstruct.create sizeof in
    Lwt.return (Request.write request buffer)
    >>*= fun _ ->
    FLOW.write flow buffer
    >>|= fun () ->
    Lwt.return (Ok ())

  let finally f g =
    let open Lwt in
    Lwt.catch
      (fun () ->
        f ()
        >>= fun ok_or_error ->
        g ();
        return ok_or_error
      ) (fun e ->
        g ();
        fail e)

  let rec allocate_fid t =
    let open Lwt in
    if t.free_fids = Types.Fid.Set.empty
    then Lwt_condition.wait t.free_fids_c >>= fun () -> allocate_fid t
    else
      let fid = Types.Fid.Set.min_elt t.free_fids in
      t.free_fids <- Types.Fid.Set.remove fid t.free_fids;
      return fid
  let deallocate_fid t fid =
    t.free_fids <- Types.Fid.Set.add fid t.free_fids;
    Lwt_condition.signal t.free_fids_c ()
  let with_fid t f =
    let open Lwt in
    allocate_fid t
    >>= fun fid ->
    finally (fun () -> f fid) (fun () -> deallocate_fid t fid)

  let rpc t request =
    (* Allocate a fresh tag, or wait if none are available yet *)
    let c = Lwt_condition.create () in
    let rec allocate_tag () =
      let open Lwt in
      if t.free_tags = Types.Tag.Set.empty
      then Lwt_condition.wait c >>= fun () -> allocate_tag ()
      else
        let tag = Types.Tag.Set.min_elt t.free_tags in
        t.free_tags <- Types.Tag.Set.remove tag t.free_tags;
        let th, wakener = Lwt.task () in
        t.wakeners <- Types.Tag.Map.add tag wakener t.wakeners;
        return (tag, th) in
    let deallocate_tag tag =
      t.free_tags <- Types.Tag.Set.add tag t.free_tags;
      t.wakeners <- Types.Tag.Map.remove tag t.wakeners;
      Lwt_condition.signal c () in
    let with_tag f =
      let open Lwt in
      allocate_tag ()
      >>= fun (tag, th) ->
      finally (fun () -> f (tag, th)) (fun () -> deallocate_tag tag) in
    with_tag
      (fun (tag, th) ->
        (* Lock the flow for output and transmit the packet *)
        Lwt_mutex.with_lock t.transmit_m
          (fun () ->
            let request = { Request.tag; payload = request } in
            write_one_packet t.flow request
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
    if not(Types.Tag.Map.mem tag t.wakeners) then begin
      let pretty_printed = Sexplib.Sexp.to_string (Response.sexp_of_t response) in
      error "Received response with unexpected tag: %s" pretty_printed;
      dispatcher_t t
    end else begin
      let wakener = Types.Tag.Map.find tag t.wakeners in
      Lwt.wakeup_later wakener response.Response.payload;
      dispatcher_t t
    end

  let return_error = function
    | Response.Err { Response.Err.ename } ->
      Lwt.return (Error (`Msg ename))
    | payload ->
      Lwt.return (error_msg "Server sent unexpected reply: %s" (Sexplib.Sexp.to_string (Response.sexp_of_payload  payload)))

  module LowLevel = struct

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

    let stat t fid =
      rpc t Request.(Stat { Stat.fid })
      >>*= function
      | Response.Stat x -> Lwt.return (Ok x)
      | response -> return_error response

    let read t fid offset count =
      rpc t Request.(Read { Read.fid; offset; count })
      >>*= function
      | Response.Read x -> Lwt.return (Ok x)
      | response -> return_error response

    let write t fid offset data =
      rpc t Request.(Write { Write.fid; offset; data })
      >>*= function
      | Response.Write x -> Lwt.return (Ok x)
      | response -> return_error response

    let clunk t fid =
      rpc t Request.(Clunk { Clunk.fid })
      >>*= function
      | Response.Clunk x -> Lwt.return (Ok x)
      | response -> return_error response

    let remove t fid =
      rpc t Request.(Remove { Remove.fid })
      >>*= function
      | Response.Remove x -> Lwt.return (Ok x)
      | response -> return_error response
  end

  let read t path offset count =
    let open LowLevel in
    let fid = t.root in
    with_fid t
      (fun newfid ->
        let wnames = path in
        walk t fid newfid wnames
        >>*= fun _ -> (* I don't need to know the qids *)
        openfid t newfid Types.Mode.Read
        >>*= fun _ ->
        let rec loop acc offset remaining =
          let to_request = min remaining t.maximum_payload in
          read t newfid offset to_request
          >>*= fun { Response.Read.data } ->
          let n = Cstruct.len data in
          if n = 0
          then Lwt.return (Ok (List.rev acc))
          else
            loop (data :: acc) Int64.(add offset (of_int n)) Int32.(sub remaining (of_int n)) in
         loop [] offset count
      )

  let stat t path =
    let open LowLevel in
    let fid = t.root in
    with_fid t
      (fun newfid ->
        let wnames = path in
        walk t fid newfid wnames
        >>*= fun _ -> (* I don't need to know the qids *)
        stat t newfid
        >>*= fun { Response.Stat.stat } ->
        Lwt.return (Ok stat)
      )

  module KV_RO = struct
    open Lwt

    type 'a io = 'a Lwt.t

    type t = connection

    type error =
      | Unknown_key of string

    type id = unit

    type page_aligned_buffer = Cstruct.t

    let parse_path x = Stringext.split x ~on:'/'

    let read t key offset length =
      let path = parse_path key in
      let offset = Int64.of_int offset in
      let count = Int32.of_int length in
      read t path offset count
      >>= function
      | Ok bufs -> return (`Ok bufs)
      | _ -> return (`Error (Unknown_key key))

    let size t key =
      let path = parse_path key in
      stat t path
      >>= function
      | Ok stat -> return (`Ok stat.Types.Stat.length)
      | _ -> return (`Error (Unknown_key key))

    let disconnect t = failwith "disconnect: unimplemented"
  end

  let readdir t path =
    let open LowLevel in
    let fid = t.root in
    with_fid t
      (fun newfid ->
        let wnames = path in
        walk t fid newfid wnames
        >>*= fun _ -> (* I don't need to know the qids *)
        openfid t newfid Types.Mode.Read
        >>*= fun _ ->
        let rec loop acc offset =
          read t newfid offset t.maximum_payload
          >>*= fun { Response.Read.data } ->
          if Cstruct.len data = 0
          then Lwt.return (Ok acc)
          else
            (* Data should be an integral number of marshalled Stat.ts *)
            let module StatArray = Types.Arr(Types.Stat) in
            (Lwt.return (StatArray.read data))
            >>*= fun (stats, rest) ->
            assert (Cstruct.len rest = 0);
            loop (acc @ stats) Int64.(add offset (of_int (Cstruct.len data))) in
        loop [] 0L
    )
  (* 8215 = 8192 + 23 (maximum overhead in a write packet) *)
  let connect flow ?(msize = 8215l) ?(username = "nobody") ?(aname = "/") () =
    write_one_packet flow {
      Request.tag = Types.Tag.notag;
      payload = Request.Version Request.Version.({ msize; version = Types.Version.default });
    } >>*= fun () ->

    (* We use the convention that fid 0l is the root fid. We'll never clunk
       this one so we can always re-explore the filesystem from the root. *)
    let root = match Types.Fid.of_int32 0l with Ok x -> x | _ -> assert false in
    let transmit_m = Lwt_mutex.create () in
    let wakeners = Types.Tag.Map.empty in
    let free_tags = Types.Tag.recommended in
    let free_fids = Types.Fid.recommended in
    let free_fids_c = Lwt_condition.create () in
    let maximum_payload = 0l in (* recomputed when the server responds *)
    let t = { flow; root; msize; maximum_payload; transmit_m; wakeners; free_tags; free_fids; free_fids_c; input_buffer = Cstruct.create 0 } in

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
        let smallest_read_response = { Response.tag = Types.Tag.notag; payload = Response.Read { Response.Read.data = Cstruct.create 0 } } in
        let maximum_read_payload = Int32.(sub msize (of_int (Response.sizeof smallest_read_response))) in
        let smallest_write_request = { Request.tag = Types.Tag.notag; payload = Request.Write { Request.Write.data = Cstruct.create 0; fid = Types.Fid.nofid; offset = 0L } } in
        let maximum_write_payload = Int32.(sub msize (of_int (Request.sizeof smallest_write_request))) in
        debug "Negotiated maximum message size: %ld bytes" msize;
        debug "Maximum read payload would be: %ld bytes" maximum_read_payload;
        debug "Maximum write payload would be: %ld bytes" maximum_write_payload;
        (* For compatibility, use the smallest of the two possible maximums *)
        let maximum_payload = min maximum_read_payload maximum_write_payload in
        debug "We will use a global maximum payload of: %ld bytes" maximum_payload;
        let t = { t with msize; maximum_payload } in
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
