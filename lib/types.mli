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
open Sexplib

val big_enough_for: string -> Cstruct.t -> int -> unit Error.t
(** [big_enough_for name buf length] returns an error with a log message
    if buffer [buf] is smaller than [length]. The [name] will be included
    in the error message. *)

module Int8 : sig
  type t = int with sexp

  include S.SERIALISABLE with type t := t
end

module Int16 : sig
  type t = int with sexp

  include S.SERIALISABLE with type t := t
end

module Int32 : sig
  include module type of Int32

  val t_of_sexp: Sexp.t -> t
  val sexp_of_t: t -> Sexp.t

  include S.SERIALISABLE with type t := t
end

module Int64 : sig
  type t = int64 with sexp

  include S.SERIALISABLE with type t := t
end

module Version : sig
  type t with sexp

  val default: t
  val unknown: t

  include S.SERIALISABLE with type t := t
end

module Fid : sig
  type t with sexp

  val nofid: t

  val of_int32: int32 -> t Error.t

  include S.SERIALISABLE with type t := t
end

module Qid : sig
  type flag =
    | Directory  (** file is a directory *)
    | AppendOnly (** writes always hit the end of the file *)
    | Exclusive  (** file is opened for exclusive use *)
    | Temporary  (** file is temporary and won't be backed up *)
  with sexp

  type t = {
   flags: flag list;
   version: int32;
   id: int64;
  } with sexp
  (** The server's unique id for the file. Two files are the same
      if and only if the Qids are the same. *)

  include S.SERIALISABLE with type t := t
end

module Tag : sig
  type t with sexp

  val notag: t
  (** The special tag value used during authentication *)

  val of_int: int -> t Error.t

  include S.SERIALISABLE with type t := t
end

module Data : sig
  type t = Cstruct.t with sexp
  (** A length-prefixed chunk of data which may include embedded NULLs, or may be
      interpreted later as UTF-8 text. *)

  val of_string: string -> t

  val to_string: t -> string

  include S.SERIALISABLE with type t := t
end

module Stat : sig
  type t = {
    ty: int;       (** for kernel use *)
    dev: int32;    (** for kernel use *)
    qid: Qid.t;
    mode: int32;   (** permissions and flags *)
    atime: int32;  (** last access time *)
    mtime: int32;  (** last modification time *)
    length: int64; (** length of the file in bytes *)
    name: string;  (** file name. Must be '/' if the file is the root *)
    uid: string;   (** owner name *)
    gid: string;   (** group name *)
    muid: string;  (** name of last user who modified the file *)
  } with sexp

  include S.SERIALISABLE with type t := t
end
