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

module Make(FLOW: V1_LWT.FLOW) = struct

  type t = {
    msize: int32;
    flow: FLOW.flow;
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

  let send_one_packet flow request =
    let sizeof = Request.sizeof request in
    let buffer = Cstruct.create sizeof in
    Lwt.return (Request.write request buffer)
    >>*= fun _ ->
    FLOW.write flow buffer
    >>|= fun () ->
    Lwt.return (Ok ())

  let connect flow ?(msize = 1024l) ?(username = "nobody") ?(aname = "/") () =
    send_one_packet flow {
      Request.tag = Types.Tag.notag;
      payload = Request.Version Request.Version.({ msize; version = Types.Version.default });
    } >>*= fun () ->

    let t = { flow; msize; input_buffer = Cstruct.create 0 } in

    read_one_packet t
    >>*= fun response ->
    match response with
    | { Response.payload = Response.Version { Response.Version.msize; version }} when version = Types.Version.default ->
      let msize = min t.msize msize in
      let t = { t with msize } in

      let tag = match Types.Tag.of_int 0 with Ok x -> x | _ -> assert false in
      let fid = match Types.Fid.of_int32 0l with Ok x -> x | _ -> assert false in
      let afid = Types.Fid.nofid in
      send_one_packet flow {
        Request.tag;
        payload = Request.Attach Request.Attach.({ fid; afid; uname = username; aname })
      } >>*= fun () ->
      read_one_packet t
      >>*= fun response ->
      begin match response with
      | { Response.payload = Response.Attach { Response.Attach.qid } } ->
        Printf.fprintf stderr "Successfully received a root qid: %s\n%!" (Sexplib.Sexp.to_string_hum (Types.Qid.sexp_of_t qid));
        Lwt.return (Ok { t with msize })
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
