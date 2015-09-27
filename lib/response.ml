(*
 * Copyright (C) 2015 David Scott <dave@recoil.org>
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

module Version = Request.Version

module Auth = struct
  type t = {
    aqid: string; (* 13 bytes long *)
  }

  let sizeof _ = 13

  let write t buf =
    let length = Cstruct.len buf in
    let needed = sizeof t in
    ( if length < needed
      then error_msg "Auth.write: buffer is too small for aqid (%d < %d)" length needed
      else return ()
    ) >>= fun () ->
    Cstruct.blit_from_string t.aqid 0 buf 0 needed;
    return ()

  let read buf =
    let length = Cstruct.len buf in
    let needed = 13 in
    ( if length < needed
      then error_msg "Auth.read: buffer is too small for aqid (%d < %d)" length needed
      else return ()
    ) >>= fun () ->
    let aqid = Cstruct.(to_string (sub buf 0 needed)) in
    return { aqid }
end

cstruct hdr {
  uint32_t size;
  uint8_t ty;
  uint16_t tag;
} as little_endian

type payload =
  | Version of Version.t
  | Auth of Auth.t

type t = {
  tag: int;
  payload: payload;
}

let sizeof t = sizeof_hdr + (match t.payload with
  | Version x -> Version.sizeof x
  | Auth x -> Auth.sizeof x
)
