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
  mutable closed: bool;
}

let connect fd =
  let read_buffer_size = 32768 in
  let read_buffer = Cstruct.create read_buffer_size in
  let closed = false in
  { fd; read_buffer_size; read_buffer; closed }

let close t =
  match t.closed with
  | false ->
    t.closed <- true;
    Lwt_unix.close t.fd
  | true ->
    Lwt.return ()

let safe op f r =
  Lwt.catch (fun () -> op f r) (function
      | Unix.Unix_error (Unix.EPIPE, _, _) -> Lwt.return 0
      | e -> Lwt.fail e)

let read flow =
  if flow.closed then Lwt.return (Ok `Eof)
  else begin
    if Cstruct.length flow.read_buffer = 0
    then flow.read_buffer <- Cstruct.create flow.read_buffer_size;
    safe Lwt_cstruct.read flow.fd flow.read_buffer >|= function
    | 0 -> Ok `Eof
    | n ->
      let result = Cstruct.sub flow.read_buffer 0 n in
      flow.read_buffer <- Cstruct.shift flow.read_buffer n;
      Ok (`Data result)
  end

let protect f =
  Lwt.catch f (function
      (* Lwt_cstruct.complete can raise End_of_file :-/ *)
      | End_of_file -> Lwt.return (Error `Closed)
      | Unix.Unix_error (e, _, _) -> Lwt.return (Error (`Unix e))
      | e -> Lwt.fail e
    )

let write flow buf =
  if flow.closed then Lwt.return (Error `Closed)
  else
    protect (fun () ->
        Lwt_cstruct.complete (safe Lwt_cstruct.write flow.fd) buf >|= fun () ->
        Ok ()
      )

let writev flow bufs =
  let rec loop = function
    | []      -> Lwt.return (Ok ())
    | x :: xs ->
      if flow.closed then Lwt.return (Error `Closed)
      else
        Lwt_cstruct.complete (safe Lwt_cstruct.write flow.fd) x >>= fun () ->
        loop xs
  in
  protect (fun () -> loop bufs)
