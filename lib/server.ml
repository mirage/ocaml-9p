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

  let write_one_packet writer request =
    debug "-> %s" (Response.to_string request);
    let sizeof = Response.sizeof request in
    let buffer = Cstruct.create sizeof in
    Lwt.return (Response.write request buffer)
    >>*= fun _ ->
    FLOW.write writer buffer
    >>|= fun () ->
    Lwt.return (Ok ())

  let connect flow ?(msize=16384l) () =
    let reader = Reader.create flow in
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
      Printf.fprintf stderr "OK\n%!";
      Lwt.return (Error (`Msg "Server unimplemented"))
    end

  let rec serve_forever t = failwith "unimplemented"
end
