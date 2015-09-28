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

open Lwt

type 'a io = 'a Lwt.t

type buffer = Cstruct.t

type error = Unix.error

let error_message = Unix.error_message

type flow = {
  fd: Lwt_unix.file_descr;
  read_buffer_size: int;
}

let connect fd =
  let read_buffer_size = 1024 in
  { fd; read_buffer_size }

let close { fd } = Lwt_unix.close fd

let read flow =
  let buffer = Lwt_bytes.create flow.read_buffer_size in
  Lwt_bytes.read flow.fd buffer 0 (Lwt_bytes.length buffer)
  >>= function
  | 0 ->
    return (`Eof)
  | n ->
    return (`Ok (Cstruct.(sub (of_bigarray buffer) 0 n)))

let write flow buf =
  Lwt_cstruct.(complete (write flow.fd) buf)
  >>= fun () ->
  return (`Ok ())

let writev flow bufs =
  let rec loop = function
    | [] -> return (`Ok ())
    | x :: xs ->
      Lwt_cstruct.(complete (write flow.fd) x)
      >>= fun () ->
      loop xs in
  loop bufs
