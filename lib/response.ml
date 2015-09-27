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
open Types

module Version = Request.Version

module Auth = struct
  type t = {
    aqid: Qid.t;
  }

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
  }

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
  type t = unit

  let sizeof _ = 0

  let write t buf = return buf

  let read buf = return ((), buf)
end

module Attach = struct
  type t = {
    qid: Qid.t
  }

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
  }

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
  }

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
  }

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

  let sizeof t = 4 + (Cstruct.len t.data)

  let write t rest =
    let len = Cstruct.len t.data in
    Int32.write (Int32.of_int len) rest
    >>= fun rest ->
    let rest = Cstruct.shift rest 4 in
    big_enough_for "Read.data" rest len
    >>= fun () ->
    Cstruct.blit t.data 0 rest 0 len;
    let rest = Cstruct.shift rest len in
    return rest

  let read rest =
    Int32.read rest
    >>= fun (len, rest) ->
    let len = Int32.to_int len in
    big_enough_for "Read.data" rest len
    >>= fun () ->
    let data = Cstruct.sub rest 0 len in (* by reference, no copy *)
    let rest = Cstruct.shift rest len in
    return ({ data }, rest)
end

module Write = struct
  type t = {
    count: int32
  }
  let sizeof _ = 4

  let write t rest =
    Int32.write t.count rest

  let read rest =
    Int32.read rest
    >>= fun (count, rest) ->
    return ({ count }, rest)
end

module Clunk = struct
  type t = unit
  let sizeof _ = 0
  let write t rest = return rest
  let read rest = return ((), rest)
end

module Remove = Clunk

module Stat = struct
  type t = {
    stat: Stat.t;
  }

  let sizeof t = Types.Stat.sizeof t.stat

  let write t rest = Types.Stat.write t.stat rest

  let read rest =
    Types.Stat.read rest
    >>= fun (stat, rest) ->
    return ( {stat}, rest )
end

module Wstat = Clunk

cstruct hdr {
  uint32_t size;
  uint8_t ty;
  uint16_t tag;
} as little_endian

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

type t = {
  tag: int;
  payload: payload;
}

let sizeof t = sizeof_hdr + (match t.payload with
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
