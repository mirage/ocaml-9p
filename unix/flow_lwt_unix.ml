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

open Lwt.Infix

type error = [`Unix of Unix.error]
type write_error = [`Closed | `Unix of Unix.error]

let pp_error ppf (`Unix e) = Fmt.string ppf (Unix.error_message e)

let pp_write_error ppf = function
  | #Mirage_flow.write_error as e -> Mirage_flow.pp_write_error ppf e
  | #error as e -> pp_error ppf e

type flow = {
  fd: Lwt_unix.file_descr;
  read_buffer_size: int;
  mutable read_buffer: Cstruct.t;
  mutable shutdown: [ `None | `Read | `Write | `Both ];
}

let connect fd =
  let read_buffer_size = 32768 in
  let read_buffer = Cstruct.create read_buffer_size in
  let shutdown = `None in
  { fd; read_buffer_size; read_buffer; shutdown }

let close t =
  match t.shutdown with
  | `None ->
    t.shutdown <- `Both;
    Lwt_unix.close t.fd
  | `Read ->
    t.shutdown <- `Both;
    Lwt_unix.shutdown t.fd Lwt_unix.SHUTDOWN_SEND;
    Lwt.return_unit
  | `Write ->
    t.shutdown <- `Both;
    Lwt_unix.shutdown t.fd Lwt_unix.SHUTDOWN_RECEIVE;
    Lwt.return_unit
  | `Both ->
    Lwt.return_unit

let safe op f r =
  Lwt.catch (fun () -> op f r) (function
      | Unix.Unix_error (Unix.EPIPE, _, _) -> Lwt.return 0
      | e -> Lwt.fail e)

let read flow =
  match flow.shutdown with
  | `Read | `Both -> Lwt.return (Ok `Eof)
  | _ ->
    if Cstruct.length flow.read_buffer = 0
    then flow.read_buffer <- Cstruct.create flow.read_buffer_size;
    safe Lwt_cstruct.read flow.fd flow.read_buffer >|= function
    | 0 -> Ok `Eof
    | n ->
      let result = Cstruct.sub flow.read_buffer 0 n in
      flow.read_buffer <- Cstruct.shift flow.read_buffer n;
      Ok (`Data result)

let protect f =
  Lwt.catch f (function
      (* Lwt_cstruct.complete can raise End_of_file :-/ *)
      | End_of_file -> Lwt.return (Error `Closed)
      | Unix.Unix_error (e, _, _) -> Lwt.return (Error (`Unix e))
      | e -> Lwt.fail e
    )

let write flow buf =
  match flow.shutdown with
  | `Write | `Both -> Lwt.return (Error `Closed)
  | _ ->
    protect (fun () ->
        Lwt_cstruct.complete (safe Lwt_cstruct.write flow.fd) buf >|= fun () ->
        Ok ()
      )

let writev flow bufs =
  let rec loop = function
    | []      -> Lwt.return (Ok ())
    | x :: xs ->
      match flow.shutdown with
      | `Write | `Both -> Lwt.return (Error `Closed)
      | _ ->
        Lwt_cstruct.complete (safe Lwt_cstruct.write flow.fd) x >>= fun () ->
        loop xs
  in
  protect (fun () -> loop bufs)

let shutdown flow cmd =
  let cmd', status = match flow.shutdown, cmd with
    | `Both, _ -> None, `Both
    | `None, x -> Some x, (match cmd with `write -> `Write | `read -> `Read | `read_write -> `Both)
    | `Read, (`write | `read_write) -> Some `write, `Both
    | `Write, (`read | `read_write) -> Some `read, `Both
    | s, _ -> None, s
  in
  let lwt_cmd = Option.map (function
    | `write -> Lwt_unix.SHUTDOWN_SEND
    | `read -> SHUTDOWN_RECEIVE
    | `read_write -> SHUTDOWN_ALL)
      cmd'
  in
  flow.shutdown <- status;
  Option.iter (Lwt_unix.shutdown flow.fd) lwt_cmd;
  Lwt.return_unit
