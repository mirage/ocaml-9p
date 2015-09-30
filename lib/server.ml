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
    mutable please_shutdown: bool;
    shutdown_complete_t: unit Lwt.t;
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

  let rec dispatcher_t shutdown_complete_wakener callback_fn t =
    if t.please_shutdown then begin
      Lwt.wakeup_later shutdown_complete_wakener ();
      Lwt.return (Ok ())
    end else begin
      read_one_packet t.reader
      >>*= fun request ->
      callback_fn request.Request.payload
      >>*= fun response_payload ->
      let response = { Response.tag = request.Request.tag; payload = response_payload } in
      write_one_packet t.writer response
      >>*= fun () ->
      dispatcher_t shutdown_complete_wakener callback_fn t
    end

  module LowLevel = struct

    let return_error writer request ename =
        write_one_packet writer {
          Response.tag = request.Request.tag;
          payload = Response.Err Response.Err.( { ename; errno = None })
        } >>*= fun () ->
        Lwt.return (Error (`Msg ename))

    let expect_version reader writer =
      Reader.read reader
      >>*= fun buffer ->
      Lwt.return (Request.read buffer)
      >>*= function
      | ( { Request.payload = Request.Version v; tag }, _) ->
        Lwt.return (Ok (tag, v))
      | request, _ ->
        return_error writer request "Expected Version message"

    let expect_attach reader writer =
      Reader.read reader
      >>*= fun buffer ->
      Lwt.return (Request.read buffer)
      >>*= function
      | ( { Request.payload = Request.Attach a; tag }, _) ->
        Lwt.return (Ok (tag, a))
      | request, _ ->
        return_error writer request "Expected Attach message"
  end

  let connect flow ?(msize=16384l) ~callback_fn () =
    let reader = Reader.create flow in
    let writer = flow in
    LowLevel.expect_version reader writer
    >>*= fun (tag, v) ->
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
      LowLevel.expect_attach reader writer
      >>*= fun (tag, a) ->
      let root = a.Request.Attach.fid in
      let root_qid = Types.Qid.file ~version:0l ~id:0L () in
      write_one_packet flow {
        Response.tag;
        payload = Response.Attach Response.Attach.({qid = root_qid })
      } >>*= fun () ->
      let please_shutdown = false in
      let shutdown_complete_t, shutdown_complete_wakener = Lwt.task () in
      let t = { reader; writer; root; root_qid; please_shutdown; shutdown_complete_t } in
      Lwt.async (fun () -> dispatcher_t shutdown_complete_wakener callback_fn t);
      Lwt.return (Ok t)
    end

  let disconnect t =
    t.please_shutdown <- true;
    t.shutdown_complete_t
end
