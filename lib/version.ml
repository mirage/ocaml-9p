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
open Result

module T = struct
  cstruct hdr {
    uint32_t msize;
    uint16_t version_len;
    (* version_len bytes *)
  } as little_endian

  type t = {
    msize: int32;
    version: string;
  }

  let sizeof t = 4 + 2 + (String.length t.version)

  let write t buf =
    let length = Cstruct.len buf in
    let needed = sizeof t in
    if length < needed
    then Error (`TooSmall(needed, length))
    else begin
      set_hdr_msize buf t.msize;
      set_hdr_version_len buf (String.length t.version);
      Ok ()
    end

  let read buf =
    let length = Cstruct.len buf in
    if length < sizeof_hdr
    then Error (`Msg "Version.read: truncated input")
    else begin
      let msize = get_hdr_msize buf in
      let version_len = get_hdr_version_len buf in
      let rest = Cstruct.shift buf sizeof_hdr in
      let remaining = Cstruct.len rest in
      if remaining < version_len
      then Error (`Msg "Version.T.unmarshal: truncated input")
      else begin
        let version = Cstruct.(to_string (sub buf sizeof_hdr version_len)) in
        Ok { msize; version }
      end
    end
end

module R = T
