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

let big_enough_for name buf needed =
  let length = Cstruct.len buf in
  if length < needed
  then error_msg "%s: buffer too small (%d < %d)" name length needed
  else return ()

module Int8 = struct
  type t = int [@@deriving sexp]

  let sizeof _ = 1

  let any = 0xFF

  let is_any x = x = any

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
  type t = int [@@deriving sexp]

  let sizeof _ = 2

  let any = 0xFFFF

  let is_any x = x = any

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

  type _t = int32 [@@deriving sexp]
  let sexp_of_t = sexp_of__t
  let t_of_sexp = _t_of_sexp

  let sizeof _ = 4

  let any = 0xFFFFFFFF_l

  let is_any x = x = any

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
  type t = int64 [@@deriving sexp]

  let sizeof _ = 8

  let any = 0xFFFFFFFFFFFFFFFF_L

  let is_any x = x = any

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
  type t = int32 [@@deriving sexp]

  let compare (a: t) (b: t) = compare a b

  module Set = Set.Make(struct type t = int32 let compare = compare end)
  module Map = Map.Make(struct type t = int32 let compare = compare end)

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
    else Ok x

  let to_int32 x = x

  let sizeof _ = 4

  let read = Int32.read
  let write = Int32.write
end

module OpenMode = struct
  type io =
    | Read
    | Write
    | ReadWrite
    | Exec
  [@@deriving sexp]

  type t = {
    io: io;
    truncate: bool;
    rclose: bool;
    append: bool;
  } [@@deriving sexp]

  let to_int { io; truncate; rclose; append } =
    let byte = match io with
      | Read -> 0
      | Write -> 1
      | ReadWrite -> 2
      | Exec -> 3
    in
    let byte = if truncate then byte lor 0x10 else byte in
    let byte = if rclose   then byte lor 0x40 else byte in
    if append then byte lor 0x80 else byte

  let all =
    to_int { io = Exec; truncate = true; rclose = true; append = true; }

  let read_only  =
    { io = Read; truncate = false; rclose = false; append = false; }
  let write_only = { read_only with io = Write; }
  let read_write = { read_only with io = ReadWrite; }
  let exec       = { read_only with io = Exec; }

  let of_int x =
    let io = match x land 3 with
      | 0 -> Read
      | 1 -> Write
      | 2 -> ReadWrite
      | 3 -> Exec
      | _ -> assert false
    in
    let truncate = x land 0x10 <> 0 in
    let rclose   = x land 0x40 <> 0 in
    let append   = x land 0x80 <> 0 in
    let extra = x land (lnot all) in
    if extra <> 0
    then error_msg "Unknown mode bits: 0x%x" extra
    else Ok { io; truncate; rclose; append; }

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
  type permission = [ `Read | `Write | `Execute ] [@@deriving sexp]

  type t = {
    owner: permission list;
    group: permission list;
    other: permission list;
    is_directory: bool;
    append_only: bool;
    exclusive: bool;
    is_mount: bool;
    is_auth: bool;
    temporary: bool;
    is_device: bool;
    is_symlink: bool;
    is_hardlink: bool;
    is_namedpipe: bool;
    is_socket: bool;
    is_setuid: bool;
    is_setgid: bool;
    is_any: bool;
  } [@@deriving sexp]

  let make ?(owner=[]) ?(group=[]) ?(other=[]) ?(is_directory=false)
    ?(append_only=false) ?(exclusive=false) ?(is_mount=false) ?(is_auth=false) ?(temporary=false)
    ?(is_device=false) ?(is_symlink=false) ?(is_hardlink=false) ?(is_namedpipe=false) ?(is_socket=false)
    ?(is_setuid=false) ?(is_setgid=false) () = {
    owner; group; other; is_directory; append_only; exclusive; is_mount;
    is_auth; temporary; is_device; is_symlink; is_hardlink; is_namedpipe; is_socket;
    is_setuid; is_setgid; is_any = false;
  }

  let any = {
    owner = []; group = []; other = [];
    is_directory = false; append_only = false; exclusive = false;
    is_mount = false; is_auth = false; temporary = false; is_device = false;
    is_symlink = false; is_hardlink = false; is_namedpipe = false; is_socket = false;
    is_setuid = false; is_setgid = false; is_any = true;
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

  let nonet_of_permissions mode =
    List.fold_left Int32.logor 0l [
      Int32.shift_left (nibble_of_permissions mode.owner) 6;
      Int32.shift_left (nibble_of_permissions mode.group) 3;
      Int32.shift_left (nibble_of_permissions mode.other) 0;
    ]

  let is_any t = t.is_any

  let read rest =
    Int32.read rest
    >>= fun (x, rest) ->
    if Int32.is_any x
    then return (any, rest)
    else
      let is_directory = is_set x 31 in
      let append_only  = is_set x 30 in
      let exclusive    = is_set x 29 in
      let is_mount     = is_set x 28 in
      let is_auth      = is_set x 27 in
      let temporary    = is_set x 26 in
      let is_symlink   = is_set x 25 in
      let is_hardlink  = is_set x 24 in
      let is_device    = is_set x 23 in
      let is_namedpipe = is_set x 21 in
      let is_socket    = is_set x 20 in
      let is_setuid    = is_set x 19 in
      let is_setgid    = is_set x 18 in
      let owner = permissions_of_nibble (Int32.shift_right x 6) in
      let group = permissions_of_nibble (Int32.shift_right x 3) in
      let other = permissions_of_nibble (Int32.shift_right x 0) in
      let t = {
        owner; group; other; is_directory; append_only; exclusive; is_mount;
        is_auth; temporary; is_device; is_symlink; is_hardlink; is_namedpipe; is_socket;
        is_setuid; is_setgid; is_any = false } in
      return (t, rest)

  let write t rest =
    if t.is_any
    then Int32.(write any rest)
    else
      let x = List.fold_left Int32.logor 0l [
        if t.is_directory then bit 31 else 0l;
        if t.append_only  then bit 30 else 0l;
        if t.exclusive    then bit 29 else 0l;
        if t.is_mount     then bit 28 else 0l;
        if t.is_auth      then bit 27 else 0l;
        if t.temporary    then bit 26 else 0l;
        if t.is_symlink   then bit 25 else 0l;
        if t.is_hardlink  then bit 24 else 0l;
        if t.is_device    then bit 23 else 0l;
        if t.is_namedpipe then bit 21 else 0l;
        if t.is_socket    then bit 20 else 0l;
        if t.is_setuid    then bit 19 else 0l;
        if t.is_setgid    then bit 18 else 0l;
        nonet_of_permissions t;
      ] in
      Int32.write x rest
end


module Qid = struct
  type flag =
    | Directory
    | AppendOnly
    | Exclusive
    | Mount
    | Auth
    | Temporary
    | Symlink
    | Link
  [@@deriving sexp]

  type t = {
   flags: flag list;
   version: int32;
   id: int64;
  } [@@deriving sexp]

  let file ?(id=(-1L)) ?(version=(-1l)) ?(append_only=false) ?(exclusive=false)
    ?(mount=false) ?(auth=false) ?(temporary=false) ?(link=false) () =
    let flags =
        (if append_only then [ AppendOnly ] else [])
      @ (if exclusive then [ Exclusive ] else [])
      @ (if mount then [ Mount ] else [])
      @ (if auth then [ Auth ] else [])
      @ (if temporary then [ Temporary ] else [])
      @ (if link then [ Link ] else []) in
    { flags; version; id }

  let dir ?(id=(-1L)) ?(version=(-1l)) () =
    let flags = [ Directory ] in
    { flags; version; id }

  let flags = [
    Directory,  0x80;
    AppendOnly, 0x40;
    Exclusive,  0x20;
    Mount,      0x10;
    Auth,       0x08;
    Temporary,  0x04;
    Symlink,    0x02;
    Link,       0x01;
  ]
  let flags' = List.map (fun (x, y) -> y, x) flags

  let to_int fs =
    List.fold_left (lor) 0 (List.map (fun x -> List.assoc x flags) fs)

  let of_int x =
    List.map snd (List.filter (fun (bit, _flag) -> x land bit <> 0) flags')

  let needed = 13

  let sizeof _ = needed

  let any = {
    flags = of_int Int8.any;
    version = Int32.any;
    id = Int64.any;
  }

  let is_any_flags flags = Int8.is_any (to_int flags)

  let is_any { flags; version; id } =
    Int32.is_any version && Int64.is_any id && is_any_flags flags

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
  type t = int option [@@deriving sexp]

  let equal (a: t) (b: t) = match a, b with
    | None, None -> true
    | Some x, Some y -> x = y
    | _ -> false

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

  let to_int = function
    | None -> -1
    | Some x -> x

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

  type _t = string [@@deriving sexp]
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
    | TwoThousandU
  [@@deriving sexp]

  let to_string = function
    | Unknown      -> "unknown"
    | TwoThousand  -> "9P2000"
    | TwoThousandU -> "9P2000.u"

  let default = TwoThousand
  let unix    = TwoThousandU
  let unknown = Unknown

  let of_string x =
    let prefix =
      try
        let dot = String.index x '.' in
        String.sub x 0 (dot - 1)
      with Not_found -> x in
    if String.length prefix >= 2 && (String.sub prefix 0 2 = "9P") then begin
      if x = to_string TwoThousandU
      then TwoThousandU
      else TwoThousand (* there may be future versions, but we don't know them *)
    end else Unknown

  let sizeof x = 2 + (String.length (to_string x))

  let read rest =
    Data.read rest
    >>= fun (x, rest) ->
    return (of_string (Data.to_string x), rest)

  let write t rest =
    Data.write (Data.of_string (to_string t)) rest
end

module Stat = struct
  type extension = {
    extension: string;
    n_uid: int32;
    n_gid: int32;
    n_muid: int32;
  } [@@deriving sexp]

  let make_extension ?(extension="") ?(n_uid=(-1l)) ?(n_gid=(-1l)) ?(n_muid=(-1l)) () =
    { extension; n_uid; n_gid; n_muid }

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
    u: extension option;
  } [@@deriving sexp]

  let make ~name ~qid ?(mode=FileMode.make ()) ?(length=0L)
    ?(atime=0l) ?(mtime=0l) ?(uid="root") ?(gid="root") ?(muid="none")
    ?u () =
    let ty = 0 and dev = 0l in
    { ty; dev; qid; mode; atime; mtime; length; name; uid; gid; muid; u }

  let sizeof t = 2 + 2 + 4 + (Qid.sizeof t.qid) + 4 + 4 + 4 + 8
    + 2 + (String.length t.name) + 2 + (String.length t.uid)
    + 2 + (String.length t.gid) + 2 + (String.length t.muid)
    + (match t.u with None -> 0 | Some x -> 2 + (String.length x.extension) + 4 + 4 + 4)

  let read buf =
    Int16.read buf
    >>= fun (sz, rest) ->
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
    let t = { ty; dev; qid; mode; atime; mtime; length; name; uid; gid; muid; u = None } in
    (* We are often decoding contiguous arrays of Stat structures,
       so we must use the sz field to discover where the data ends. *)
    let consumed = Cstruct.len buf - (Cstruct.len rest) in
    if consumed = sz + 2 (* Size of the sz field itself *)
    then return (t, rest)
    else
      Data.read rest
      >>= fun (x, rest) ->
      let extension = Data.to_string x in
      Int32.read rest
      >>= fun (n_uid, rest) ->
      Int32.read rest
      >>= fun (n_gid, rest) ->
      Int32.read rest
      >>= fun (n_muid, rest) ->
      let u = Some { extension; n_uid; n_gid; n_muid } in
      (* In case of future extensions, remove trailing garbage *)
      let consumed = Cstruct.len buf - (Cstruct.len rest) in
      let trailing_garbage = consumed - sz - 2 in
      let rest = Cstruct.shift rest trailing_garbage in
      return ({ t with u }, rest)

  let write t rest =
    let len = sizeof t in
    Int16.write (len - 2) rest
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
    >>= fun rest ->
    match t.u with
    | None -> return rest
    | Some x ->
      Data.(write (of_string x.extension) rest)
      >>= fun rest ->
      Int32.write x.n_uid rest
      >>= fun rest ->
      Int32.write x.n_gid rest
      >>= fun rest ->
      Int32.write x.n_muid rest

end

module Arr(T: Protocol_9p_s.SERIALISABLE) = struct
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
