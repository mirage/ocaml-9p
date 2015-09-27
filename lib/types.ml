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

let big_enough_for name buf needed =
  let length = Cstruct.len buf in
  if length < needed
  then error_msg "%s: buffer too small (%d < %d)" name length needed
  else return ()

module Int8 = struct
  type t = int

  let sizeof _ = 1

  let read buf =
    big_enough_for "Int8.read" buf 1
    >>= fun () ->
    return (Cstruct.get_uint8 buf 0, Cstruct.shift buf 1)

  let write t buf =
    big_enough_for "Int8.write" buf 1
    >>= fun () ->
    Cstruct.set_uint8 buf 0 t;
    return (Cstruct.shift buf 1)
end

module Int16 = struct
  type t = int

  let sizeof _ = 2

  let read buf =
    big_enough_for "Int16.read" buf 2
    >>= fun () ->
    return (Cstruct.LE.get_uint16 buf 0, Cstruct.shift buf 2)

  let write t buf =
    big_enough_for "Int16.write" buf 2
    >>= fun () ->
    Cstruct.LE.set_uint16 buf 0 t;
    return (Cstruct.shift buf 2)
end

module Int32 = struct
  include Int32

  let sizeof _ = 4

  let read buf =
    big_enough_for "Int32.read" buf 4
    >>= fun () ->
    return (Cstruct.LE.get_uint32 buf 0, Cstruct.shift buf 4)

  let write t buf =
    big_enough_for "Int32.read" buf 4
    >>= fun () ->
    Cstruct.LE.set_uint32 buf 0 t;
    return (Cstruct.shift buf 4)
end

module Int64 = struct
  type t = int64

  let sizeof _ = 8

  let read buf =
    big_enough_for "Int64.read" buf 8
    >>= fun () ->
    return (Cstruct.LE.get_uint64 buf 0, Cstruct.shift buf 8)

  let write t buf =
    big_enough_for "Int64.read" buf 8
    >>= fun () ->
    Cstruct.LE.set_uint64 buf 0 t;
    return (Cstruct.shift buf 8)
end

module Qid = struct
  type t = string

  let needed = 13

  let sizeof _ = needed

  let write t buf =
    big_enough_for "Qid.write" buf needed
    >>= fun () ->
    Cstruct.blit_from_string t 0 buf 0 needed;
    return (Cstruct.shift buf needed)

  let read buf =
    big_enough_for "Qid.read" buf needed
    >>= fun () ->
    let qid = Cstruct.(to_string (sub buf 0 needed)) in
    return (qid, Cstruct.shift buf needed)

end

module Data = struct
  type t = Cstruct.t

  let of_string x =
    let t = Cstruct.create (String.length x) in
    Cstruct.blit_from_string x 0 t 0 (String.length x);
    t

  let to_string = Cstruct.to_string

  let sizeof t = 2 + (Cstruct.len t)

  let read buf =
    let length = Cstruct.len buf in
    ( if length < 2
      then error_msg "Buffer is too short to contain a string length"
      else return ()
    ) >>= fun () ->
    let required = Cstruct.LE.get_uint16 buf 0 in
    let rest = Cstruct.shift buf 2 in
    let remaining = Cstruct.len rest in
    ( if remaining < required
      then error_msg "Buffer is too short to contain string payload"
      else return ()
    ) >>= fun () ->
    let data = Cstruct.sub rest 0 required in
    let rest = Cstruct.shift rest required in
    return (data, rest)

  let write t buf =
    let length = Cstruct.len buf in
    let needed = sizeof t in
    ( if needed < length
      then error_msg "Buffer is too small for Data.t (%d < %d)" needed length
      else return ()
    ) >>= fun () ->
    Cstruct.LE.set_uint16 buf 0 (Cstruct.len t);
    Cstruct.blit t 0 buf 2 (Cstruct.len t);
    return (Cstruct.shift buf needed)
end
