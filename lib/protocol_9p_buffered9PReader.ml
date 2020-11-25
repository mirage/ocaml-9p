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

open Rresult
open Protocol_9p_error
open Lwt.Infix

let max_message_size = 655360l       (* 640 KB should be enough... Linux limit is 32 KB *)

module Make(Log: Protocol_9p_s.LOG)(FLOW: Mirage_flow.S) = struct
  module C = Mirage_channel.Make(FLOW)
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

  let read_exactly ~len c =
    C.read_exactly ~len c >>= function
    | Ok (`Data bufs) -> Lwt.return (Ok (Cstruct.concat bufs))
    | Ok `Eof -> Lwt.return (Error `Eof)
    | Error e -> Lwt.return (Error (`Msg (Fmt.strf "%a" C.pp_error e)))

  let read_must_have_lock t =
    let len_size = 4 in
    read_exactly ~len:len_size t.channel >>= function
    | Ok length_buffer -> begin
        match Cstruct.LE.get_uint32 length_buffer 0 with
        | bad_length when bad_length < Int32.of_int len_size
                       || bad_length > max_message_size ->
            Lwt.return (error_msg "Message size %lu out of range" bad_length)
        | length -> begin
          read_exactly ~len:(Int32.to_int length - len_size) t.channel >>= function
          | Ok packet_buffer -> Lwt.return (Ok packet_buffer)
          | err -> Lwt.return err
        end
    end
    | Error e -> Lwt.return (Error e)

  let read t =
    Lwt_mutex.with_lock t.read_m (fun () ->
      read_must_have_lock t >|= function
      | Ok _ as ok -> ok
      | Error `Eof -> error_msg "Caught EOF on underlying FLOW"
      | Error (`Msg _) as err ->
        R.reword_error_msg (fun msg ->
            R.msgf "Unexpected error on underlying FLOW: %s" msg) err
    )
end
