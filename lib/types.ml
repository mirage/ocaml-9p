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

  let nofid = 0xffffffffl

  let of_int32 x =
    if x = nofid
    then error_msg "%ld is an invalid fid (it is defined to be NOFID in the spec)" x
    else Result.Ok x

  let sizeof _ = 4

  let read = Int32.read
  let write = Int32.write
end

module Mode = struct
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

module Qid = struct
  type flag = Directory | AppendOnly | Exclusive | Temporary with sexp

  type t = {
   flags: flag list;
   version: int32;
   id: int64;
  } with sexp

  let flags = [
    Directory,  0x80;
    AppendOnly, 0x40;
    Exclusive,  0x20;
    Temporary,  0x04
  ]
  let flags' = List.map (fun (x, y) -> y, x) flags

  let needed = 13

  let sizeof _ = needed

  let write t rest =
    big_enough_for "Qid.write" rest needed
    >>= fun () ->
    let ty = List.fold_left (lor) 0 (List.map (fun x -> List.assoc x flags) t.flags) in
    Int8.write ty rest
    >>= fun rest ->
    Int32.write t.version rest
    >>= fun rest ->
    Int64.write t.id rest

  let read rest =
    big_enough_for "Qid.read" rest needed
    >>= fun () ->
    Int8.read rest
    >>= fun (ty, rest) ->
    let flags = List.map snd (List.filter (fun (bit, flag) -> ty land bit <> 0) flags') in
    Int32.read rest
    >>= fun (version, rest) ->
    Int64.read rest
    >>= fun (id, rest) ->
    let qid = { flags; version; id } in
    return (qid, rest)

end

module Tag = struct
  type t = int option with sexp

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

  let compare (a: t) (b: t) = match a, b with
    | None, None -> 0
    | Some _, None -> -1
    | None, Some _ -> 1
    | Some x, Some y -> compare x y
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
    mode: int32;
    atime: int32;
    mtime: int32;
    length: int64;
    name: string;
    uid: string;
    gid: string;
    muid: string;
  } with sexp

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
    Int32.read rest
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
    Int32.write t.mode rest
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
