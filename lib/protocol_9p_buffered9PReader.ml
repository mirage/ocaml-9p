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
open Protocol_9p_error
open Protocol_9p_infix

let max_message_size = 655360l       (* 640 KB should be enough... Linux limit is 32 KB *)

module Make(Log: Protocol_9p_s.LOG)(FLOW: V1_LWT.FLOW) = struct
  type t = {
    flow: FLOW.flow;
    read_m: Lwt_mutex.t;
    mutable input_buffer: Cstruct.t;
  }

  let create flow =
    let read_m = Lwt_mutex.create () in
    let input_buffer = Cstruct.create 0 in
    { flow; read_m; input_buffer }

  (* For converting flow errors *)
  let (>>|=) m f =
    let open Lwt in
    m >>= function
    | `Ok x -> f x
    | `Eof -> return (error_msg "Caught EOF on underlying FLOW")
    | `Error e -> return (error_msg "Unexpected error on underlying FLOW: %s" (FLOW.error_message e))

  let read_into t output =
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

  let read_must_have_lock t =
    let len_size = 4 in
    read_into t (Cstruct.create len_size)
    >>*= fun length_buffer ->
    match Cstruct.LE.get_uint32 length_buffer 0 with
    | bad_length when bad_length < Int32.of_int len_size
                   || bad_length > max_message_size ->
        Lwt.return (error_msg "Message size %lu out of range" bad_length)
    | length ->
    let packet_buffer = Cstruct.create (Int32.to_int length) in
    read_into t (Cstruct.shift packet_buffer len_size)
    >>*= fun _packet_body ->
    Cstruct.blit length_buffer 0 packet_buffer 0 len_size;
    Lwt.return (Ok packet_buffer)

  let read t = Lwt_mutex.with_lock t.read_m (fun () -> read_must_have_lock t)
end
