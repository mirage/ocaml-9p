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
open Lwt.Infix

let max_message_size = 655360l       (* 640 KB should be enough... Linux limit is 32 KB *)

module Make(Log: Protocol_9p_s.LOG)(FLOW: V1_LWT.FLOW) = struct
  module C = Channel.Make(FLOW)
  type t = {
    channel: C.t;
    read_m: Lwt_mutex.t;
    mutable input_buffer: Cstruct.t;
  }

  let create flow =
    let channel = C.create flow in
    let read_m = Lwt_mutex.create () in
    let input_buffer = Cstruct.create 0 in
    { channel; read_m; input_buffer }

  let read_exactly ~len t =
    let rec loop acc = function
      | 0 -> Lwt.return @@ Cstruct.concat @@ List.rev acc
      | len ->
        C.read_some ~len t
        >>= fun buffer ->
        loop (buffer :: acc) (len - (Cstruct.len buffer)) in
    loop [] len

  let read_must_have_lock t =
    let len_size = 4 in
    Lwt.catch
      (fun () ->
        read_exactly ~len:len_size t.channel
        >>= fun length_buffer ->
        match Cstruct.LE.get_uint32 length_buffer 0 with
        | bad_length when bad_length < Int32.of_int len_size
                       || bad_length > max_message_size ->
            Lwt.return (error_msg "Message size %lu out of range" bad_length)
        | length ->
        read_exactly ~len:(Int32.to_int length - len_size) t.channel
        >>= fun packet_buffer ->
        Lwt.return (Ok packet_buffer)
      ) (function
        | End_of_file -> Lwt.return (error_msg "Caught EOF on underlying FLOW")
        | C.Read_error e -> Lwt.return (error_msg "Unexpected error on underlying FLOW: %s" (FLOW.error_message e))
        | e -> Lwt.fail e
      )
  let read t = Lwt_mutex.with_lock t.read_m (fun () -> read_must_have_lock t)
end
