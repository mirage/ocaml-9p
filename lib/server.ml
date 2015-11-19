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
open Infix

type info = {
  root: Types.Fid.t;
  version: Types.Version.t;
}

type receive_cb = info -> cancel:unit Lwt.t -> Request.payload -> Response.payload Error.t Lwt.t

module Make(Log: S.LOG)(FLOW: V1_LWT.FLOW) = struct
  module Reader = Buffered9PReader.Make(Log)(FLOW)
  open Log

  type t = {
    write_lock : Lwt_mutex.t;
    reader: Reader.t;
    writer: FLOW.flow;
    info: info;
    root_qid: Types.Qid.t;
    (* Press the "cancel button" by setting the ref to true (if present in the
       map) *)
    mutable cancel_buttons: unit Lwt.u Types.Tag.Map.t;
    mutable please_shutdown: bool;
    shutdown_complete_t: unit Lwt.t;
  }

  let get_info t = t.info

  (* For converting flow errors *)
  let (>>|=) m f =
    let open Lwt in
    m >>= function
    | `Ok x -> f x
    | `Eof -> return (error_msg "Caught EOF on underlying FLOW")
    | `Error e -> return (error_msg "Unexpected error on underlying FLOW: %s" (FLOW.error_message e))

  let disconnect t =
    t.please_shutdown <- true;
    t.shutdown_complete_t

  let after_disconnect t = t.shutdown_complete_t

  let write_one_packet ?write_lock writer response =
    if !Log.print_debug then debug "S %a" Response.pp response;
    let sizeof = Response.sizeof response in
    let buffer = Cstruct.create sizeof in
    Lwt.return (Response.write response buffer)
    >>*= fun _ ->
    ( match write_lock with
      | Some m ->
        Lwt_mutex.with_lock m (fun () -> FLOW.write writer buffer)
      | None ->
        FLOW.write writer buffer )
    >>|= fun () ->
    Lwt.return (Ok ())

  let read_one_packet reader =
    let open Lwt in
    Reader.read reader
    >>= function
    | Error (`Msg _) as e -> Lwt.return e
    | Ok buffer ->
      Lwt.return begin
        match Request.read buffer with
        | Error (`Msg ename) ->
          Error (`Parse (ename, buffer))
        | Ok (request, _) ->
          if !Log.print_debug then debug "C %a" Request.pp request;
          Ok request
      end

  let error_response tag ename = {
    Response.tag;
    payload = Response.(Err {
      Err.ename;
      errno = None;
    });
  }

  let rec dispatcher_t shutdown_complete_wakener receive_cb t =
    if t.please_shutdown then begin
      Lwt.wakeup_later shutdown_complete_wakener ();
      Lwt.return (Ok ())
    end else begin
      let open Lwt in
      read_one_packet t.reader
      >>= function
      | Error (`Msg message) ->
        debug "S error reading: %s" message;
        debug "Disconnecting client";
        disconnect t
        >>= fun () ->
        dispatcher_t shutdown_complete_wakener receive_cb t
      | Error (`Parse (ename, buffer)) -> begin
          match Request.read_header buffer with
          | Error (`Msg _) ->
            debug "C sent bad header: %s" ename;
            dispatcher_t shutdown_complete_wakener receive_cb t
          | Ok (_, _, tag, _) ->
            debug "C error: %s" ename;
            let response = error_response tag ename in
            write_one_packet ~write_lock:t.write_lock t.writer response
            >>*= fun () ->
            dispatcher_t shutdown_complete_wakener receive_cb t
        end
      | Ok { Request.tag; payload = Request.Flush { Request.Flush.oldtag } } ->
        Lwt_mutex.with_lock t.write_lock
          (fun () ->
            if Types.Tag.Map.mem oldtag t.cancel_buttons then begin
              let cancel_u = Types.Tag.Map.find oldtag t.cancel_buttons in
              Lwt.wakeup_later cancel_u ();
              debug "S will suppress response for tag %s" (Sexplib.Sexp.to_string (Types.Tag.sexp_of_t oldtag));
              t.cancel_buttons <- Types.Tag.Map.remove oldtag t.cancel_buttons;
            end;
            write_one_packet t.writer { Response.tag; payload = Response.Flush () }
          )
          >>= begin function
          | Ok () ->
            dispatcher_t shutdown_complete_wakener receive_cb t
          | Error (`Msg m) ->
            disconnect t
            >>= fun () ->
            Lwt.return (Error (`Msg m))
          end
      | Ok request ->
        let cancel_t, cancel_u = Lwt.task () in
        t.cancel_buttons <- Types.Tag.Map.add request.Request.tag cancel_u t.cancel_buttons;
        Lwt.async (fun () ->
          receive_cb t.info ~cancel:cancel_t request.Request.payload
          >>= begin function
            | Error (`Msg message) ->
              Lwt.return (error_response request.Request.tag message)
            | Ok response_payload ->
              Lwt.return {
                Response.tag = request.Request.tag;
                payload = response_payload;
              }
          end >>= fun response ->
          Lwt_mutex.with_lock t.write_lock
            (fun () ->
              if Lwt.state cancel_t = Lwt.Sleep then begin
                (* It's safe to unbind the tag because the flush hasn't been
                   transmitted yet. *)
                t.cancel_buttons <- Types.Tag.Map.remove request.Request.tag t.cancel_buttons;
                write_one_packet t.writer response
              end else begin
                Lwt.return (Ok ())
              end
          )
          >>= begin function
            | Error (`Msg message) ->
              debug "S error writing: %s" message;
              debug "Disconnecting client";
              disconnect t
            | Ok () -> Lwt.return ()
          end
        );
        dispatcher_t shutdown_complete_wakener receive_cb t
    end

  module LowLevel = struct

    let return_error ~write_lock writer request ename =
        write_one_packet ~write_lock writer {
          Response.tag = request.Request.tag;
          payload = Response.Err Response.Err.( { ename; errno = None })
        } >>*= fun () ->
        Lwt.return (Error (`Msg ename))

    let expect_version ~write_lock reader writer =
      Reader.read reader
      >>*= fun buffer ->
      Lwt.return (Request.read buffer)
      >>*= function
      | ( { Request.payload = Request.Version v; tag }, _) ->
        Lwt.return (Ok (tag, v))
      | request, _ ->
        return_error ~write_lock writer request "Expected Version message"

    let expect_attach ~write_lock reader writer =
      Reader.read reader
      >>*= fun buffer ->
      Lwt.return (Request.read buffer)
      >>*= function
      | ( { Request.payload = Request.Attach a; tag }, _) ->
        Lwt.return (Ok (tag, a))
      | request, _ ->
        return_error ~write_lock writer request "Expected Attach message"
  end

  let connect flow ?(msize=16384l) ~receive_cb () =
    let write_lock = Lwt_mutex.create () in
    let reader = Reader.create flow in
    let writer = flow in
    LowLevel.expect_version ~write_lock reader writer
    >>*= fun (tag, v) ->
    let msize = min msize v.Request.Version.msize in
    if v.Request.Version.version = Types.Version.unknown then begin
      error "Client sent a 9P version string we couldn't understand";
      Lwt.return (Error (`Msg "Received unknown 9P version string"))
    end else begin
      let version = v.Request.Version.version in
      write_one_packet ~write_lock flow {
        Response.tag;
        payload = Response.Version Response.Version.({ msize; version });
      } >>*= fun () ->
      info "Using protocol version %s" (Sexplib.Sexp.to_string (Types.Version.sexp_of_t version));
      LowLevel.expect_attach ~write_lock reader writer
      >>*= fun (tag, a) ->
      let root = a.Request.Attach.fid in
      let info = { root; version } in
      let root_qid = Types.Qid.dir ~version:0l ~id:0L () in
      let cancel_buttons = Types.Tag.Map.empty in
      write_one_packet ~write_lock flow {
        Response.tag;
        payload = Response.Attach Response.Attach.({qid = root_qid })
      } >>*= fun () ->
      let please_shutdown = false in
      let shutdown_complete_t, shutdown_complete_wakener = Lwt.task () in
      let t = { reader; writer; info; root_qid; cancel_buttons; please_shutdown; shutdown_complete_t; write_lock } in
      Lwt.async (fun () -> dispatcher_t shutdown_complete_wakener receive_cb t);
      Lwt.return (Ok t)
    end
end
