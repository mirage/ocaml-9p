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
open Error
open Result

module Make(Log: S.LOG)(FLOW: V1_LWT.FLOW) = struct
  module Reader = Buffered9PReader.Make(Log)(FLOW)
  open Log

  type t = {
    reader: Reader.t;
    writer: FLOW.flow;
    root: Types.Fid.t;
    root_qid: Types.Qid.t;
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

  let write_one_packet writer response =
    debug "-> %s" (Response.to_string response);
    let sizeof = Response.sizeof response in
    let buffer = Cstruct.create sizeof in
    Lwt.return (Response.write response buffer)
    >>*= fun _ ->
    FLOW.write writer buffer
    >>|= fun () ->
    Lwt.return (Ok ())

  let read_one_packet reader =
    Reader.read reader
    >>*= fun buffer ->
    Lwt.return (Request.read buffer)
    >>*= fun (request, _) ->
    debug "-> %s" (Request.to_string request);
    Lwt.return (Ok request)

  let connect flow ?(msize=16384l) () =
    let reader = Reader.create flow in
    let writer = flow in
    Reader.read reader
    >>*= fun buffer ->
    Lwt.return (Request.read buffer)
    >>*= function ( { Request.payload = Request.Version v; tag }, _) ->
    let msize = min msize v.Request.Version.msize in
    if v.Request.Version.version = Types.Version.unknown then begin
      error "Client sent a 9P version string we couldn't understand";
      Lwt.return (Error (`Msg "Received unknown 9P version string"))
    end else begin
      write_one_packet flow {
        Response.tag;
        payload = Response.Version Response.Version.({ msize; version = v.Request.Version.version });
      } >>*= fun () ->
      info "Using protocol version %s" (Sexplib.Sexp.to_string (Types.Version.sexp_of_t v.Request.Version.version));
      Reader.read reader
      >>*= fun buffer ->
      Lwt.return (Request.read buffer)
      >>*= function ( { Request.payload = Request.Attach a; tag }, _) ->
      let root = a.Request.Attach.fid in
      let root_qid = Types.Qid.file ~version:0l ~id:0L () in
      write_one_packet flow {
        Response.tag;
        payload = Response.Attach Response.Attach.({qid = root_qid })
      } >>*= fun () ->
      Printf.fprintf stderr "OK\n%!";
      Lwt.return (Ok { reader; writer; root; root_qid })
    end

  let rec serve_forever t = failwith "unimplemented"
end
