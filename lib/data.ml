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

type t = Cstruct.t

let of_string x =
  let t = Cstruct.create (String.length x) in
  Cstruct.blit_from_string x 0 t 0 (String.length x);
  t

let read buf =
  let length = Cstruct.len buf in
  if length < 2
  then Error(`Msg "Buffer is too short to contain a string length")
  else begin
    let required = Cstruct.LE.get_uint16 buf 0 in
    let rest = Cstruct.shift buf 2 in
    let remaining = Cstruct.len rest in
    if remaining < required
    then Error(`Msg "Buffer is too short to contain string payload")
    else begin
      let payload = Cstruct.sub rest 0 required in
      let trailing = Cstruct.shift rest required in
      Ok(payload, trailing)
    end
  end
