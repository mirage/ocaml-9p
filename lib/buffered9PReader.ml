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
open Error
open Infix

module Make(Log: S.LOG)(FLOW: V1_LWT.FLOW) = struct
  open Log

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

  let read_exactly t n =
    let output = Cstruct.create n in
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
    read_exactly t 4
    >>*= fun length_buffer ->
    let length = Cstruct.LE.get_uint32 length_buffer 0 in
    read_exactly t (Int32.to_int length - 4)
    >>*= fun packet_buffer ->
    (* XXX: remove this data copy *)
    let buffer = Cstruct.create (Cstruct.len length_buffer + (Cstruct.len packet_buffer)) in
    Cstruct.blit length_buffer 0 buffer 0 (Cstruct.len length_buffer);
    Cstruct.blit packet_buffer 0 buffer (Cstruct.len length_buffer) (Cstruct.len packet_buffer);
    Lwt.return (Ok buffer)

  let read t = Lwt_mutex.with_lock t.read_m (fun () -> read_must_have_lock t)
end
