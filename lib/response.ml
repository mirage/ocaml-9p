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
open Types

module Version = Request.Version

module Auth = struct
  type t = {
    aqid: Qid.t;
  } with sexp

  let sizeof t = Qid.sizeof t.aqid

  let write t buf = Qid.write t.aqid buf

  let read buf =
    Qid.read buf
    >>= fun (aqid, rest) ->
    return ( { aqid }, rest )
end

module Err = struct
  type t = {
    ename: string;
  } with sexp

  let sizeof t = 2 + (String.length t.ename)

  let write t buf =
    let ename = Data.of_string t.ename in
    Data.write ename buf

  let read buf =
    Data.read buf
    >>= fun (ename, rest) ->
    let ename = Data.to_string ename in
    return ({ ename }, rest)
end

module Flush = struct
  type t = unit with sexp

  let sizeof _ = 0

  let write t buf = return buf

  let read buf = return ((), buf)
end

module Attach = struct
  type t = {
    qid: Qid.t
  } with sexp

  let sizeof t = Qid.sizeof t.qid

  let write t buf = Qid.write t.qid buf

  let read buf =
    Qid.read buf
    >>= fun (qid, rest) ->
    return ({ qid }, rest)
end

module Walk = struct
  type t = {
    wqids: Qid.t list
  } with sexp

  let sizeof t = 2 + (List.fold_left (+) 0 (List.map (fun q -> Qid.sizeof q) t.wqids))

  let write t rest =
    Int16.write (List.length t.wqids) rest
    >>= fun rest ->
    let rec loop rest = function
      | [] -> return rest
      | wqid :: wqids ->
        Qid.write wqid rest
        >>= fun rest ->
        loop rest wqids in
    loop rest t.wqids

  let read rest =
    Int16.read rest
    >>= fun (nwqids, rest) ->
    let rec loop rest acc = function
      | 0 -> return (List.rev acc, rest)
      | n ->
        Qid.read rest
        >>= fun (wqid, rest) ->
        loop rest (wqid :: acc) (n - 1) in
    loop rest [] nwqids
    >>= fun (wqids, rest) ->
    return ( { wqids }, rest )
end

module Open = struct
  type t = {
    qid: Qid.t;
    iounit: int32
  } with sexp

  let sizeof t = Qid.sizeof t.qid + 4

  let write t rest =
    Qid.write t.qid rest
    >>= fun rest ->
    Int32.write t.iounit rest

  let read rest =
    Qid.read rest
    >>= fun (qid, rest) ->
    Int32.read rest
    >>= fun (iounit, rest) ->
    return ({ qid; iounit }, rest)
end

module Create = struct
  type t = {
    qid: Qid.t;
    iounit: int32;
  } with sexp

  let sizeof t = Qid.sizeof t.qid + 4

  let write t rest =
    Qid.write t.qid rest
    >>= fun rest ->
    Int32.write t.iounit rest

  let read rest =
    Qid.read rest
    >>= fun (qid, rest) ->
    Int32.read rest
    >>= fun (iounit, rest) ->
    return ( {qid; iounit}, rest)
end

module Read = struct
  type t = {
    data: Cstruct.t
  }

  type _t = string with sexp
  let sexp_of_t t = sexp_of__t (Cstruct.to_string t.data)
  let t_of_sexp s =
    let _t = _t_of_sexp s in
    let len = String.length _t in
    let buf = Cstruct.create len in
    Cstruct.blit_from_string _t 0 buf 0 len;
    { data = buf }

  let sizeof t = 4 + (Cstruct.len t.data)

  let write t rest =
    let len = Cstruct.len t.data in
    Int32.write (Int32.of_int len) rest
    >>= fun rest ->
    big_enough_for "Read.data" rest len
    >>= fun () ->
    Cstruct.blit t.data 0 rest 0 len;
    let rest = Cstruct.shift rest len in
    return rest

  let read rest =
    Int32.read rest
    >>= fun (len, rest) ->
    let len = Int32.to_int len in
Printf.fprintf stderr "len = %d\n%!" len;
    big_enough_for "Read.data" rest len
    >>= fun () ->
    let data = Cstruct.sub rest 0 len in (* by reference, no copy *)
    let rest = Cstruct.shift rest len in
    return ({ data }, rest)
end

module Write = struct
  type t = {
    count: int32
  } with sexp
  let sizeof _ = 4

  let write t rest =
    Int32.write t.count rest

  let read rest =
    Int32.read rest
    >>= fun (count, rest) ->
    return ({ count }, rest)
end

module Clunk = struct
  type t = unit with sexp
  let sizeof _ = 0
  let write t rest = return rest
  let read rest = return ((), rest)
end

module Remove = Clunk

module Stat = struct
  type t = {
    stat: Stat.t;
  } with sexp

  let sizeof t = 2 + (Types.Stat.sizeof t.stat)

  let write t rest =
    Int16.write (Types.Stat.sizeof t.stat) rest
    >>= fun rest ->
    Types.Stat.write t.stat rest

  let read rest =
    Int16.read rest
    >>= fun (_len, rest) ->
    Types.Stat.read rest
    >>= fun (stat, rest) ->
    return ( {stat}, rest )
end

module Wstat = Clunk

type payload =
  | Version of Version.t
  | Auth of Auth.t
  | Err of Err.t
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
  tag: Types.Tag.t;
  payload: payload;
} with sexp

let sizeof t = 4 + 1 + 2 + (match t.payload with
  | Version x -> Version.sizeof x
  | Auth x -> Auth.sizeof x
  | Err x -> Err.sizeof x
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
  big_enough_for "Response" rest needed
  >>= fun () ->
  let ty = match t.payload with
    | Version _ -> 101
    | Auth _    -> 103
    | Err _     -> 107
    | Flush _   -> 109
    | Attach _  -> 105
    | Walk _    -> 111
    | Open _    -> 113
    | Create _  -> 115
    | Read _    -> 117
    | Write _   -> 119
    | Clunk _   -> 121
    | Remove _  -> 123
    | Stat _    -> 125
    | Wstat _   -> 127 in
  Int32.write (Int32.of_int needed) rest
  >>= fun rest ->
  Int8.write ty rest
  >>= fun rest ->
  Tag.write t.tag rest
  >>= fun rest ->
  match t.payload with
    | Version x -> Version.write x rest
    | Auth x -> Auth.write x rest
    | Err x -> Err.write x rest
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
  Tag.read rest
  >>= fun (tag, rest) ->
  ( match ty with
    | 101 -> Version.read rest >>= fun (x, rest) -> return ((Version x), rest)
    | 103 -> Auth.read rest    >>= fun (x, rest) -> return ((Auth x), rest)
    | 107 -> Err.read rest     >>= fun (x, rest) -> return ((Err x), rest)
    | 109 -> Flush.read rest   >>= fun (x, rest) -> return ((Flush x), rest)
    | 105 -> Attach.read rest  >>= fun (x, rest) -> return ((Attach x), rest)
    | 111 -> Walk.read rest    >>= fun (x, rest) -> return ((Walk x), rest)
    | 113 -> Open.read rest    >>= fun (x, rest) -> return ((Open x), rest)
    | 115 -> Create.read rest  >>= fun (x, rest) -> return ((Create x), rest)
    | 117 -> Read.read rest    >>= fun (x, rest) -> return ((Read x), rest)
    | 119 -> Write.read rest   >>= fun (x, rest) -> return ((Write x), rest)
    | 121 -> Clunk.read rest   >>= fun (x, rest) -> return ((Clunk x), rest)
    | 123 -> Remove.read rest  >>= fun (x, rest) -> return ((Remove x), rest)
    | 125 -> Stat.read rest    >>= fun (x, rest) -> return ((Stat x), rest)
    | 127 -> Wstat.read rest   >>= fun (x, rest) -> return ((Wstat x), rest)
    | ty  -> error_msg "Response.read: unknown packet type %d" ty
  ) >>= fun (payload, rest) ->
  return ( { tag; payload }, rest )

let to_string t = Sexplib.Sexp.to_string_hum (sexp_of_t t)
