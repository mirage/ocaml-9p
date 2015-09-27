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
open Sexplib.Std
open Result
open Types
open Error

module Version = struct
  type t = {
    msize: int32;
    version: string;
  } with sexp

  let sizeof t = 4 + 2 + (String.length t.version)

  let write t rest =
    Int32.write t.msize rest
    >>= fun rest ->
    let version = Data.of_string t.version in
    Data.write version rest

  let read rest =
    Int32.read rest
    >>= fun (msize, rest) ->
    Data.read rest
    >>= fun (version, rest) ->
    let version = Data.to_string version in
    return ({ msize; version }, rest)
end

module Auth = struct

  type t = {
    afid: int32;
    uname: string;
    aname: string;
  } with sexp

  let sizeof t = 4 + 2 + (String.length t.uname) + 2 + (String.length t.aname)

  let write t rest =
    Int32.write t.afid rest
    >>= fun rest ->
    let uname = Data.of_string t.uname in
    Data.write uname rest
    >>= fun rest ->
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
  } with sexp

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
  } with sexp

  let sizeof t = 4 + 4 + 2 + (String.length t.uname) + 2 + (String.length t.aname)

  let write t rest =
    Int32.write t.fid rest
    >>= fun rest ->
    Int32.write t.afid rest
    >>= fun rest ->
    let uname = Data.of_string t.uname in
    Data.write uname rest
    >>= fun rest ->
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

module Walk = struct

  type t = {
    fid: int32;
    newfid: int32;
    wnames: string list;
  } with sexp

  let sizeof t = 4 + 4 + 2 + (List.fold_left (+) 0 (List.map (fun x -> 2 + (String.length x)) t.wnames))

  let write t rest =
    Int32.write t.fid rest
    >>= fun rest ->
    Int32.write t.newfid rest
    >>= fun rest ->
    Int16.write (List.length t.wnames) rest
    >>= fun rest ->
    let rec loop rest = function
      | [] -> return rest
      | wname :: wnames ->
        let wname = Data.of_string wname in
        Data.write wname rest
        >>= fun rest ->
        loop rest wnames in
    loop rest t.wnames

  let read rest =
    Int32.read rest
    >>= fun (fid, rest) ->
    Int32.read rest
    >>= fun (newfid, rest) ->
    Int16.read rest
    >>= fun (length, rest) ->
    let rec loop rest acc = function
      | 0 -> return (List.rev acc, rest)
      | n ->
        Data.read rest
        >>= fun (wname, rest) ->
        let wname = Data.to_string wname in
        loop rest (wname :: acc) (n - 1) in
    loop rest [] length
    >>= fun (wnames, rest) ->
    return ( { fid; newfid; wnames }, rest)
end

module Open = struct
  type t = {
    fid: int32;
    mode: int;
  } with sexp

  let sizeof _ = 5

  let write t rest =
    Int32.write t.fid rest
    >>= fun rest ->
    Int8.write t.mode rest

  let read rest =
    Int32.read rest
    >>= fun (fid, rest) ->
    Int8.read rest
    >>= fun (mode, rest) ->
    return ({ fid; mode }, rest)
end

module Create = struct
  type t = {
    fid: int32;
    name: string;
    perm: int32;
    mode: int
  } with sexp

  let sizeof t = 4 + 2 + (String.length t.name) + 4 + 1

  let write t rest =
    Int32.write t.fid rest
    >>= fun rest ->
    let name = Data.of_string t.name in
    Data.write name rest
    >>= fun rest ->
    Int32.write t.perm rest
    >>= fun rest ->
    Int8.write t.mode rest

  let read rest =
    Int32.read rest
    >>= fun (fid, rest) ->
    Data.read rest
    >>= fun (name, rest) ->
    Int32.read rest
    >>= fun (perm, rest) ->
    Int8.read rest
    >>= fun (mode, rest) ->
    let name = Data.to_string name in
    return ({ fid; name; perm; mode}, rest)
end

module Read = struct
  type t = {
    fid: int32;
    offset: int64;
    count: int32;
  } with sexp

  let sizeof _ = 4 + 8 + 4

  let write t rest =
    Int32.write t.fid rest
    >>= fun rest ->
    Int64.write t.offset rest
    >>= fun rest ->
    Int32.write t.count rest

  let read rest =
    Int32.read rest
    >>= fun (fid, rest) ->
    Int64.read rest
    >>= fun (offset, rest) ->
    Int32.read rest
    >>= fun (count, rest) ->
    return ({ fid; offset; count}, rest)

end

module Write = struct
  type t = {
    fid: int32;
    offset: int64;
    data: Cstruct.t;
  } with sexp

  let sizeof t = 4 + 8 + 4 + (Cstruct.len t.data)

  let write t rest =
    Int32.write t.fid rest
    >>= fun rest ->
    Int64.write t.offset rest
    >>= fun rest ->
    let len = Cstruct.len t.data in
    Int32.write (Int32.of_int len) rest
    >>= fun rest ->
    big_enough_for "Write.data" rest len
    >>= fun () ->
    Cstruct.blit t.data 0 rest 0 len;
    let rest = Cstruct.shift rest len in
    return rest

  let read rest =
    Int32.read rest
    >>= fun (fid, rest) ->
    Int64.read rest
    >>= fun (offset, rest) ->
    Int32.read rest
    >>= fun (len, rest) ->
    let len = Int32.to_int len in
    big_enough_for "Write.data" rest len
    >>= fun () ->
    let data = Cstruct.sub rest 0 len in
    let rest = Cstruct.shift rest len in
    return ({ fid; offset; data }, rest)
end

module Clunk = struct
  type t = {
    fid: int32
  } with sexp

  let sizeof _ = 4

  let write t rest = Int32.write t.fid rest

  let read rest =
    Int32.read rest
    >>= fun (fid, rest) ->
    return ( { fid }, rest )
end

module Remove = Clunk

module Stat = Clunk

module Wstat = struct
  type t = {
    fid: int32;
    stat: Types.Stat.t;
  } with sexp

  let sizeof t = 4 + (Types.Stat.sizeof t.stat)

  let write t rest =
    Int32.write t.fid rest
    >>= fun rest ->
    Types.Stat.write t.stat rest

  let read rest =
    Int32.read rest
    >>= fun (fid, rest) ->
    Types.Stat.read rest
    >>= fun (stat, rest) ->
    return ( { fid; stat }, rest )
end

type payload =
  | Version of Version.t
  | Auth of Auth.t
  | Flush of Flush.t
  | Attach of Attach.t
  | Walk of Walk.t
  | Open of Open.t
  | Create of Create.t
  | Read of Read.t
  | Write of Write.t
  | Clunk of Clunk.t
  | Remove of Remove.t
  | Stat of Stat.t
  | Wstat of Wstat.t
with sexp

type t = {
  tag: int;
  payload: payload
} with sexp

let sizeof t = 4 + 1 + 2 + (match t.payload with
  | Version x -> Version.sizeof x
  | Auth x -> Auth.sizeof x
  | Flush x -> Flush.sizeof x
  | Attach x -> Attach.sizeof x
  | Walk x -> Walk.sizeof x
  | Open x -> Open.sizeof x
  | Create x -> Create.sizeof x
  | Read x -> Read.sizeof x
  | Write x -> Write.sizeof x
  | Clunk x -> Clunk.sizeof x
  | Remove x -> Remove.sizeof x
  | Stat x -> Stat.sizeof x
  | Wstat x -> Wstat.sizeof x
)

let write t rest =
  let needed = sizeof t in
  big_enough_for "Request" rest needed
  >>= fun () ->
  let ty = match t.payload with
    | Version _ -> 100
    | Auth _    -> 102
    | Flush _   -> 108
    | Attach _  -> 104
    | Walk _    -> 110
    | Open _    -> 112
    | Create _  -> 114
    | Read _    -> 116
    | Write _   -> 118
    | Clunk _   -> 120
    | Remove _  -> 122
    | Stat _    -> 124
    | Wstat _   -> 126 in
  Int32.write (Int32.of_int needed) rest
  >>= fun rest ->
  Int8.write ty rest
  >>= fun rest ->
  Int16.write t.tag rest
  >>= fun rest ->
  match t.payload with
    | Version x -> Version.write x rest
    | Auth x -> Auth.write x rest
    | Flush x -> Flush.write x rest
    | Attach x -> Attach.write x rest
    | Walk x -> Walk.write x rest
    | Open x -> Open.write x rest
    | Create x -> Create.write x rest
    | Read x -> Read.write x rest
    | Write x -> Write.write x rest
    | Clunk x -> Clunk.write x rest
    | Remove x -> Remove.write x rest
    | Stat x -> Stat.write x rest
    | Wstat x -> Wstat.write x rest

let read rest =
  Int32.read rest
  >>= fun (len, rest) ->
  Int8.read rest
  >>= fun (ty, rest) ->
  Int16.read rest
  >>= fun (tag, rest) ->
  ( match ty with
    | 100 -> Version.read rest >>= fun (x, rest) -> return ((Version x), rest)
    | 102 -> Auth.read rest    >>= fun (x, rest) -> return ((Auth x), rest)
    | 108 -> Flush.read rest   >>= fun (x, rest) -> return ((Flush x), rest)
    | 104 -> Attach.read rest  >>= fun (x, rest) -> return ((Attach x), rest)
    | 110 -> Walk.read rest    >>= fun (x, rest) -> return ((Walk x), rest)
    | 112 -> Open.read rest    >>= fun (x, rest) -> return ((Open x), rest)
    | 114 -> Create.read rest  >>= fun (x, rest) -> return ((Create x), rest)
    | 116 -> Read.read rest    >>= fun (x, rest) -> return ((Read x), rest)
    | 118 -> Write.read rest   >>= fun (x, rest) -> return ((Write x), rest)
    | 120 -> Clunk.read rest   >>= fun (x, rest) -> return ((Clunk x), rest)
    | 122 -> Remove.read rest  >>= fun (x, rest) -> return ((Remove x), rest)
    | 124 -> Stat.read rest    >>= fun (x, rest) -> return ((Stat x), rest)
    | 126 -> Wstat.read rest   >>= fun (x, rest) -> return ((Wstat x), rest)
    | ty  -> error_msg "Request.read: unknown packet type %d" ty
  ) >>= fun (payload, rest) ->
  return ( { tag; payload }, rest )

let to_string t = Sexplib.Sexp.to_string_hum (sexp_of_t t)
