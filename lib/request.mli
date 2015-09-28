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

module Version : sig

  type t = {
    msize: int32;
    version: string;
  } with sexp
  (** The payload of a version message *)

  include S.SERIALISABLE with type t := t
end

module Auth : sig

  type t = {
    afid: int32;
    uname: string;
    aname: string;
  } with sexp
  (** The payload of a version message *)

  include S.SERIALISABLE with type t := t
end

module Flush : sig

  type t = {
    oldtag: int;
  } with sexp
  (** The payload of a flush message *)

  include S.SERIALISABLE with type t := t
end

module Attach : sig

  type t = {
    fid: int32;
    afid: int32;
    uname: string;
    aname: string;
  } with sexp
  (** The payload of an attach message *)

  include S.SERIALISABLE with type t := t
end

module Walk : sig

  type t = {
    fid: int32;
    newfid: int32;
    wnames: string list;
  } with sexp
  (** The payload of a walk message *)

  include S.SERIALISABLE with type t := t
end

module Open : sig
  type t = {
    fid: int32;
    mode: int;
  } with sexp
  (** The payload of an Open message *)

  include S.SERIALISABLE with type t := t
end

module Create : sig
  type t = {
    fid: int32;
    name: string;
    perm: int32;
    mode: int
  } with sexp
  (** The payload of a Create message *)

  include S.SERIALISABLE with type t := t
end

module Read : sig
  type t = {
    fid: int32;
    offset: int64;
    count: int32;
  } with sexp
  (** The payload of a Read message *)

  include S.SERIALISABLE with type t := t
end

module Write : sig
  type t = {
    fid: int32;
    offset: int64;
    data: Cstruct.t;
  } with sexp

  include S.SERIALISABLE with type t := t
end

module Clunk : sig
  type t = {
    fid: int32
  } with sexp

  include S.SERIALISABLE with type t := t
end

module Remove : sig
  type t = {
    fid: int32
  } with sexp

  include S.SERIALISABLE with type t := t
end

module Stat : sig
  type t = {
    fid: int32
  } with sexp

  include S.SERIALISABLE with type t := t
end

module Wstat : sig
  type t = {
    fid: int32;
    stat: Types.Stat.t;
  } with sexp

  include S.SERIALISABLE with type t := t
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
  tag: Types.Tag.t;
  payload: payload;
} with sexp

include S.SERIALISABLE with type t := t

val to_string: t -> string
