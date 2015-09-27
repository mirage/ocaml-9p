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
open Types
open Error

module Version = struct
  type t = {
    msize: int32;
    version: string;
  }

  let sizeof t = 4 + 2 + (String.length t.version)

  let write t rest =
    Int32.write t.msize rest
    >>= fun () ->
    let rest = Cstruct.shift rest (Int32.sizeof t.msize) in
    let version = Data.of_string t.version in
    Data.write version rest

  let read rest =
    Int32.read rest
    >>= fun (msize, rest) ->
    Data.read rest
    >>= fun (version, _) ->
    let version = Data.to_string version in
    return ({ msize; version }, rest)
end

module Auth = struct

  type t = {
    afid: int32;
    uname: string;
    aname: string;
  }

  let sizeof t = 4 + 2 + (String.length t.uname) + 2 + (String.length t.aname)

  let write t rest =
    Int32.write t.afid rest
    >>= fun () ->
    let rest = Cstruct.shift rest (Int32.sizeof t.afid) in
    let uname = Data.of_string t.uname in
    Data.write uname rest
    >>= fun () ->
    let rest = Cstruct.shift rest (Data.sizeof uname) in
    let aname = Data.of_string t.aname in
    Data.write aname rest

  let read rest =
    Int32.read rest
    >>= fun (afid, rest) ->
    Data.read rest
    >>= fun (uname, rest) ->
    Data.read rest
    >>= fun (aname, rest) ->
    let uname = Data.to_string uname in
    let aname = Data.to_string aname in
    return ({ afid; uname; aname }, rest)
end

module Flush = struct
  type t = {
    oldtag: int;
  }

  let sizeof _ = 2

  let write t rest =
    Int16.write t.oldtag rest

  let read buf =
    Int16.read buf
    >>= fun (oldtag, rest) ->
    return ({ oldtag }, rest)
end

module Attach = struct
  type t = {
    fid: int32;
    afid: int32;
    uname: string;
    aname: string;
  }

  let sizeof t = 4 + 4 + 2 + (String.length t.uname) + 2 + (String.length t.aname)

  let write t rest =
    Int32.write t.fid rest
    >>= fun () ->
    let rest = Cstruct.shift rest (Int32.sizeof t.fid) in
    Int32.write t.afid rest
    >>= fun () ->
    let rest = Cstruct.shift rest (Int32.sizeof t.afid) in
    let uname = Data.of_string t.uname in
    Data.write uname rest
    >>= fun () ->
    let rest = Cstruct.shift rest (Data.sizeof uname) in
    let aname = Data.of_string t.aname in
    Data.write aname rest

  let read rest =
    Int32.read rest
    >>= fun (fid, rest) ->
    Int32.read rest
    >>= fun (afid, rest) ->
    Data.read rest
    >>= fun (uname, rest) ->
    Data.read rest
    >>= fun (aname, rest) ->
    let uname = Data.to_string uname in
    let aname = Data.to_string aname in
    return ({ fid; afid; uname; aname }, rest)
end

cstruct hdr {
  uint32_t size;
  uint8_t ty;
  uint16_t tag;
} as little_endian

type payload =
  | Version of Version.t
  | Auth of Auth.t
  | Flush of Flush.t
  | Attach of Attach.t

type t = {
  tag: int;
  payload: payload
}

let sizeof t = sizeof_hdr + (match t.payload with
  | Version x -> Version.sizeof x
  | Auth x -> Auth.sizeof x
  | Flush x -> Flush.sizeof x
  | Attach x -> Attach.sizeof x
)
