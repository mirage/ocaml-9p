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
open Error

let big_enough_for name buf needed =
  let length = Cstruct.len buf in
  if length < needed
  then error_msg "%s: buffer too small (%d < %d)" name length needed
  else return ()

module Int8 = struct
  type t = int with sexp

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
  type t = int with sexp

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

  type _t = int32 with sexp
  let sexp_of_t = sexp_of__t
  let t_of_sexp = _t_of_sexp

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
  type t = int64 with sexp

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

module Fid = struct
  type t = int32 with sexp

  let compare (a: t) (b: t) = compare a b

  module Set = Set.Make(struct type t = int32 let compare = compare end)

  let recommended =
    let rec loop acc = function
      | 0 -> acc
      | tag ->
        loop (Set.add (Int32.of_int tag) acc) (tag - 1) in
    loop Set.empty 100

  let nofid = 0xffffffffl

  let of_int32 x =
    if x = nofid
    then error_msg "%ld is an invalid fid (it is defined to be NOFID in the spec)" x
    else Result.Ok x

  let sizeof _ = 4

  let read = Int32.read
  let write = Int32.write
end

module OpenMode = struct
  type t = Read | Write | ReadWrite | Exec with sexp

  let to_int = function
    | Read      -> 0
    | Write     -> 1
    | ReadWrite -> 2
    | Exec      -> 3

  let of_int = function
    | 0 -> Result.Ok Read
    | 1 -> Result.Ok Write
    | 2 -> Result.Ok ReadWrite
    | 3 -> Result.Ok Exec
    | n -> error_msg "Unknown mode number: %d" n

  let sizeof _ = 1

  let read rest =
    Int8.read rest
    >>= fun (x, rest) ->
    of_int x
    >>= fun mode ->
    return (mode, rest)

  let write t rest = Int8.write (to_int t) rest
end

module FileMode = struct
  type permission = [ `Read | `Write | `Execute ] with sexp

  type t = {
    owner: permission list;
    group: permission list;
    other: permission list;
    is_directory: bool;
    append_only: bool;
    exclusive: bool;
    is_auth: bool;
    temporary: bool;
  } with sexp

  let make ?(owner=[]) ?(group=[]) ?(other=[]) ?(is_directory=false)
    ?(append_only=false) ?(exclusive=false) ?(is_auth=false) ?(temporary=false) () = {
    owner; group; other; is_directory; append_only; exclusive;
    is_auth; temporary;
  }

  let sizeof _ = 4

  let is_set x n = Int32.(logand x (shift_left 1l n) <> 0l)

  let permissions_of_nibble x =
      ( if is_set x 2 then [ `Read ] else [] )
    @ ( if is_set x 1 then [ `Write ] else [] )
    @ ( if is_set x 0 then [ `Execute ] else [] )

  let bit = Int32.shift_left 1l

  let nibble_of_permissions permissions =
    let to_nibble = function
      | `Read    -> bit 2
      | `Write   -> bit 1
      | `Execute -> bit 0 in
    List.fold_left Int32.logor 0l (List.map to_nibble permissions)

  let read rest =
    Int32.read rest
    >>= fun (x, rest) ->
    let is_directory = is_set x 31 in
    let append_only  = is_set x 30 in
    let exclusive    = is_set x 29 in
    let is_auth      = is_set x 27 in
    let temporary    = is_set x 26 in
    let owner = permissions_of_nibble (Int32.shift_right x 6) in
    let group = permissions_of_nibble (Int32.shift_right x 3) in
    let other = permissions_of_nibble (Int32.shift_right x 0) in
    let t = { owner; group; other; is_directory; append_only; exclusive; is_auth; temporary } in
    return (t, rest)

  let write t rest =
    let x = List.fold_left Int32.logor 0l [
      if t.is_directory then bit 31 else 0l;
      if t.append_only  then bit 30 else 0l;
      if t.exclusive    then bit 29 else 0l;
      if t.is_auth      then bit 27 else 0l;
      if t.temporary    then bit 26 else 0l;
      Int32.shift_left (nibble_of_permissions t.owner) 6;
      Int32.shift_left (nibble_of_permissions t.group) 3;
      Int32.shift_left (nibble_of_permissions t.other) 0;
    ] in
    Int32.write x rest
end


module Qid = struct
  type flag = Directory | AppendOnly | Exclusive | Temporary with sexp

  type t = {
   flags: flag list;
   version: int32;
   id: int64;
  } with sexp

  let file ?(id=(-1L)) ?(version=(-1l)) ?(append_only=false) ?(exclusive=false) ?(temporary=false) () =
    let flags =
        (if append_only then [ AppendOnly ] else [])
      @ (if exclusive then [ Exclusive ] else [])
      @ (if temporary then [ Temporary ] else []) in
    { flags; version; id }

  let dir ?(id=(-1L)) ?(version=(-1l)) () =
    let flags = [ Directory ] in
    { flags; version; id }

  let flags = [
    Directory,  0x80;
    AppendOnly, 0x40;
    Exclusive,  0x20;
    Temporary,  0x04
  ]
  let flags' = List.map (fun (x, y) -> y, x) flags

  let to_int fs =
    List.fold_left (lor) 0 (List.map (fun x -> List.assoc x flags) fs)

  let of_int x =
    List.map snd (List.filter (fun (bit, flag) -> x land bit <> 0) flags')

  let needed = 13

  let sizeof _ = needed

  let write t rest =
    big_enough_for "Qid.write" rest needed
    >>= fun () ->
    Int8.write (to_int t.flags) rest
    >>= fun rest ->
    Int32.write t.version rest
    >>= fun rest ->
    Int64.write t.id rest

  let read rest =
    big_enough_for "Qid.read" rest needed
    >>= fun () ->
    Int8.read rest
    >>= fun (ty, rest) ->
    let flags = of_int ty in
    Int32.read rest
    >>= fun (version, rest) ->
    Int64.read rest
    >>= fun (id, rest) ->
    let qid = { flags; version; id } in
    return (qid, rest)

end

module Tag = struct
  type t = int option with sexp

  let compare (a: t) (b: t) = match a, b with
    | None, None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
    | Some x, Some y -> compare x y

  module Set = Set.Make(struct type t = int option let compare = compare end)
  module Map = Map.Make(struct type t = int option let compare = compare end)

  let recommended =
    let rec loop acc = function
      | 0 -> acc
      | tag ->
        loop (Set.add (Some tag) acc) (tag - 1) in
    loop Set.empty 100

  let of_int x =
    if x >= 0xffff || x < 0
    then error_msg "Valid tags must be between 0 <= tag < 0xffff (not %d)" x
    else return (Some x)

  let notag = None

  let needed = 2

  let sizeof _ = needed

  let write t buf =
    big_enough_for "Tag.write" buf needed
    >>= fun () ->
    let x = match t with
      | None -> 0xffff
      | Some x -> x in
    Cstruct.LE.set_uint16 buf 0 x;
    return (Cstruct.shift buf needed)

  let read buf =
    big_enough_for "Tag.read" buf needed
    >>= fun () ->
    let x = match Cstruct.LE.get_uint16 buf 0 with
      | 0xffff -> None
      | x -> Some x in
    return (x, Cstruct.shift buf needed)

end

module Data = struct
  type t = Cstruct.t

  type _t = string with sexp
  let sexp_of_t t = sexp_of__t (Cstruct.to_string t)
  let t_of_sexp s =
    let _t = _t_of_sexp s in
    let len = String.length _t in
    let buf = Cstruct.create len in
    Cstruct.blit_from_string _t 0 buf 0 len;
    buf

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
    ( if length < needed
      then error_msg "Buffer is too small for Data.t (%d < %d)" needed length
      else return ()
    ) >>= fun () ->
    Cstruct.LE.set_uint16 buf 0 (Cstruct.len t);
    Cstruct.blit t 0 buf 2 (Cstruct.len t);
    return (Cstruct.shift buf needed)
end


module Version = struct
  type t =
    | Unknown
    | TwoThousand
  with sexp

  let to_string = function
    | Unknown     -> "unknown"
    | TwoThousand -> "9P2000"

  let default = TwoThousand
  let unknown = Unknown

  let of_string x =
    let prefix =
      try
        let dot = String.index x '.' in
        String.sub x 0 (dot - 1)
      with Not_found -> x in
    if String.length prefix >= 2 && (String.sub prefix 0 2 = "9P")
    then TwoThousand (* there may be future versions, but we don't know them *)
    else Unknown

  let sizeof x = 2 + (String.length (to_string x))

  let read rest =
    Data.read rest
    >>= fun (x, rest) ->
    return (of_string (Data.to_string x), rest)

  let write t rest =
    Data.write (Data.of_string (to_string t)) rest
end

module Stat = struct
  type t = {
    ty: int;
    dev: int32;
    qid: Qid.t;
    mode: FileMode.t;
    atime: int32;
    mtime: int32;
    length: int64;
    name: string;
    uid: string;
    gid: string;
    muid: string;
  } with sexp

  let make ~name ~qid ?(mode=FileMode.make ()) ?(length=0L) ?(atime=0l) ?(mtime=0l) ?(uid="root") ?(gid="root") ?(muid="none") () =
    let ty = 0 and dev = 0l in
    { ty; dev; qid; mode; atime; mtime; length; name; uid; gid; muid }

  let sizeof t = 2 + 2 + 4 + (Qid.sizeof t.qid) + 4 + 4 + 4 + 8
    + 2 + (String.length t.name) + 2 + (String.length t.uid)
    + 2 + (String.length t.gid) + 2 + (String.length t.muid)

  let read rest =
    Int16.read rest
    >>= fun (_len, rest) ->
    Int16.read rest
    >>= fun (ty, rest) ->
    Int32.read rest
    >>= fun (dev, rest) ->
    Qid.read rest
    >>= fun (qid, rest) ->
    FileMode.read rest
    >>= fun (mode, rest) ->
    Int32.read rest
    >>= fun (atime, rest) ->
    Int32.read rest
    >>= fun (mtime, rest) ->
    Int64.read rest
    >>= fun (length, rest) ->
    Data.read rest
    >>= fun (name, rest) ->
    let name = Data.to_string name in
    Data.read rest
    >>= fun (uid, rest) ->
    let uid = Data.to_string uid in
    Data.read rest
    >>= fun (gid, rest) ->
    let gid = Data.to_string gid in
    Data.read rest
    >>= fun (muid, rest) ->
    let muid = Data.to_string muid in
    return ( { ty; dev; qid; mode; atime; mtime; length; name; uid; gid; muid }, rest)

  let write t rest =
    let len = sizeof t in
    Int16.write len rest
    >>= fun rest ->
    Int16.write t.ty rest
    >>= fun rest ->
    Int32.write t.dev rest
    >>= fun rest ->
    Qid.write t.qid rest
    >>= fun rest ->
    FileMode.write t.mode rest
    >>= fun rest ->
    Int32.write t.atime rest
    >>= fun rest ->
    Int32.write t.mtime rest
    >>= fun rest ->
    Int64.write t.length rest
    >>= fun rest ->
    let name = Data.of_string t.name in
    let uid = Data.of_string t.uid in
    let gid = Data.of_string t.gid in
    let muid = Data.of_string t.muid in
    Data.write name rest
    >>= fun rest ->
    Data.write uid rest
    >>= fun rest ->
    Data.write gid rest
    >>= fun rest ->
    Data.write muid rest

end

module Arr(T: S.SERIALISABLE) = struct
  type t = T.t list

  let rec sizeof = function
    | [] -> 0
    | t :: ts -> T.sizeof t + (sizeof ts)

  let read rest =
    let rec loop acc rest =
      if Cstruct.len rest = 0
      then return (List.rev acc, rest)
      else
        T.read rest
        >>= fun (t, rest) ->
        loop (t :: acc) rest in
    loop [] rest

  let write ts rest =
    let rec loop rest = function
      | [] -> return rest
      | t :: ts ->
        T.write t rest
        >>= fun rest ->
        loop rest ts in
    loop rest ts
end
