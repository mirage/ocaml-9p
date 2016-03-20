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
open Protocol_9p_error

module Types = Protocol_9p_types
open Types

module Version = struct
  type t = {
    msize: int32;
    version: Types.Version.t;
  } [@@deriving sexp]

  let sizeof t = 4 + (Types.Version.sizeof t.version)

  let write t rest =
    Int32.write t.msize rest
    >>= fun rest ->
    Types.Version.write t.version rest

  let read rest =
    Int32.read rest
    >>= fun (msize, rest) ->
    Types.Version.read rest
    >>= fun (version, rest) ->
    return ({ msize; version }, rest)
end

module Auth = struct

  type t = {
    afid: Fid.t;
    uname: string;
    aname: string;
    n_uname: int32 option;
  } [@@deriving sexp]

  let sizeof t =
    (Fid.sizeof t.afid) + 2 +
    (String.length t.uname) + 2 +
    (String.length t.aname)
    + (match t.n_uname with Some _ -> 4 | None -> 0)

  let write t rest =
    Fid.write t.afid rest
    >>= fun rest ->
    let uname = Data.of_string t.uname in
    Data.write uname rest
    >>= fun rest ->
    let aname = Data.of_string t.aname in
    Data.write aname rest
    >>= fun rest ->
    match t.n_uname with
    | None -> return rest
    | Some x -> Int32.write x rest

  let read rest =
    Fid.read rest
    >>= fun (afid, rest) ->
    Data.read rest
    >>= fun (uname, rest) ->
    Data.read rest
    >>= fun (aname, rest) ->
    let uname = Data.to_string uname in
    let aname = Data.to_string aname in
    if Cstruct.len rest = 0
    then return ({ afid; uname; aname; n_uname = None }, rest)
    else
      Int32.read rest
      >>= fun (x, rest) ->
      return ({ afid; uname; aname; n_uname = Some x }, rest)

end

module Flush = struct
  type t = {
    oldtag: Types.Tag.t;
  } [@@deriving sexp]

  let sizeof _ = 2

  let write t rest =
    Types.Tag.write t.oldtag rest

  let read buf =
    Types.Tag.read buf
    >>= fun (oldtag, rest) ->
    return ({ oldtag }, rest)
end

module Attach = struct
  type t = {
    fid: Fid.t;
    afid: Fid.t;
    uname: string;
    aname: string;
    n_uname: int32 option;
  } [@@deriving sexp]

  let sizeof t =
    (Fid.sizeof t.fid) + (Fid.sizeof t.afid) + 2 +
    (String.length t.uname) + 2 + (String.length t.aname)
    + (match t.n_uname with Some _ -> 4 | None -> 0)

  let write t rest =
    Fid.write t.fid rest
    >>= fun rest ->
    Fid.write t.afid rest
    >>= fun rest ->
    let uname = Data.of_string t.uname in
    Data.write uname rest
    >>= fun rest ->
    let aname = Data.of_string t.aname in
    Data.write aname rest
    >>= fun rest ->
    match t.n_uname with
    | None -> return rest
    | Some x -> Int32.write x rest

  let read rest =
    Fid.read rest
    >>= fun (fid, rest) ->
    Fid.read rest
    >>= fun (afid, rest) ->
    Data.read rest
    >>= fun (uname, rest) ->
    Data.read rest
    >>= fun (aname, rest) ->
    let uname = Data.to_string uname in
    let aname = Data.to_string aname in
    if Cstruct.len rest = 0
    then return ({ fid; afid; uname; aname; n_uname = None }, rest)
    else
      Int32.read rest
      >>= fun (x, rest) ->
      return ({ fid; afid; uname; aname; n_uname = Some x }, rest)

end

module Walk = struct

  type t = {
    fid: Fid.t;
    newfid: Fid.t;
    wnames: string list;
  } [@@deriving sexp]

  let sizeof t =
    (Fid.sizeof t.fid) + (Fid.sizeof t.newfid) + 2 +
    (List.fold_left (+) 0 (List.map (fun x -> 2 + (String.length x)) t.wnames))

  let write t rest =
    Fid.write t.fid rest
    >>= fun rest ->
    Fid.write t.newfid rest
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
    Fid.read rest
    >>= fun (fid, rest) ->
    Fid.read rest
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
    fid: Fid.t;
    mode: OpenMode.t;
  } [@@deriving sexp]

  let sizeof _ = 5

  let write t rest =
    Fid.write t.fid rest
    >>= fun rest ->
    OpenMode.write t.mode rest

  let read rest =
    Fid.read rest
    >>= fun (fid, rest) ->
    OpenMode.read rest
    >>= fun (mode, rest) ->
    return ({ fid; mode }, rest)
end

module Create = struct
  type t = {
    fid: Fid.t;
    name: string;
    perm: FileMode.t;
    mode: OpenMode.t;
    extension: string option;
  } [@@deriving sexp]

  let sizeof t =
    (Fid.sizeof t.fid) + 2 + (String.length t.name) + 4 + 1
    + (match t.extension with Some x -> 2 + (String.length x) | None -> 0)

  let write t rest =
    Fid.write t.fid rest
    >>= fun rest ->
    let name = Data.of_string t.name in
    Data.write name rest
    >>= fun rest ->
    FileMode.write t.perm rest
    >>= fun rest ->
    OpenMode.write t.mode rest
    >>= fun rest ->
    match t.extension with
    | None -> return rest
    | Some x -> Data.(write (of_string x) rest)

  let read rest =
    Fid.read rest
    >>= fun (fid, rest) ->
    Data.read rest
    >>= fun (name, rest) ->
    FileMode.read rest
    >>= fun (perm, rest) ->
    OpenMode.read rest
    >>= fun (mode, rest) ->
    let name = Data.to_string name in
    if Cstruct.len rest = 0
    then return ({ fid; name; perm; mode; extension = None}, rest)
    else
      Data.read rest
      >>= fun (x, rest) ->
      return ({ fid; name; perm; mode; extension = Some (Data.to_string x)}, rest)
end

module Read = struct
  type t = {
    fid: Fid.t;
    offset: int64;
    count: int32;
  } [@@deriving sexp]

  let sizeof t = (Fid.sizeof t.fid) + 8 + 4

  let write t rest =
    Fid.write t.fid rest
    >>= fun rest ->
    Int64.write t.offset rest
    >>= fun rest ->
    Int32.write t.count rest

  let read rest =
    Fid.read rest
    >>= fun (fid, rest) ->
    Int64.read rest
    >>= fun (offset, rest) ->
    Int32.read rest
    >>= fun (count, rest) ->
    return ({ fid; offset; count}, rest)

end

module Write = struct
  type t = {
    fid: Fid.t;
    offset: int64;
    data: Cstruct.t;
  } [@@deriving sexp]

  let equal a b =
    a.fid = b.fid && a.offset = b.offset && Cstruct.equal a.data b.data

  let sizeof_header = 4 + 8 + 4

  let sizeof t = (Fid.sizeof t.fid) + 8 + 4 + (Cstruct.len t.data)

  let write t rest =
    Fid.write t.fid rest
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
    Fid.read rest
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
    fid: Fid.t
  } [@@deriving sexp]

  let sizeof t = Fid.sizeof t.fid

  let write t rest = Fid.write t.fid rest

  let read rest =
    Fid.read rest
    >>= fun (fid, rest) ->
    return ( { fid }, rest )
end

module Remove = Clunk

module Stat = Clunk

module Wstat = struct
  type t = {
    fid: Fid.t;
    stat: Types.Stat.t;
  } [@@deriving sexp]

  let sizeof t = (Fid.sizeof t.fid) + 2 + (Types.Stat.sizeof t.stat)

  let write t rest =
    Fid.write t.fid rest
    >>= fun rest ->
    Int16.write (Types.Stat.sizeof t.stat) rest
    >>= fun rest ->
    Types.Stat.write t.stat rest

  let read rest =
    Fid.read rest
    >>= fun (fid, rest) ->
    Int16.read rest
    >>= fun (_len, rest) ->
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
[@@deriving sexp]

let equal_payload a b = match a, b with
  | Write a, Write b -> Write.equal a b
  | _ -> a = b

type t = {
  tag: Types.Tag.t;
  payload: payload
} [@@deriving sexp]


let equal a b =
  Types.Tag.equal a.tag b.tag
  && equal_payload a.payload b.payload

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
  Tag.write t.tag rest
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

let read_header rest =
  Int32.read rest
  >>= fun (len, rest) ->
  Int8.read rest
  >>= fun (ty, rest) ->
  Tag.read rest
  >>= fun (tag, rest) ->
  return (len, ty, tag, rest)

let sizeof_header = 4 + 1 + 2

let read rest =
  read_header rest
  >>= fun (len, ty, tag, rest) ->
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
    | ty  -> error_msg "unknown packet type %d" ty
  ) >>= fun (payload, rest) ->
  return ( { tag; payload }, rest )

let pp ppf = function
  | { tag; payload = Write { Write.fid; offset; data } } ->
    let tag = Types.Tag.to_int tag in
    let fid = Types.Fid.to_int32 fid in
    let len = Cstruct.len data in
    Format.fprintf ppf "tag %d Write(fid: %ld, offset: %Ld, len(data): %d)"
      tag fid offset len
  | t -> Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
