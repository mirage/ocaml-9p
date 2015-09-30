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

(** Parsers and printers for types used in 9P messages *)

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
  val unix: t    (** The extension known as 9P2000.u *)
  val unknown: t

  include S.SERIALISABLE with type t := t
end

module Fid : sig
  type t with sexp

  module Set : Set.S with type elt = t

  val nofid: t

  val of_int32: int32 -> t Error.t

  val recommended: Set.t
  (** A list of recommended fids. The client can allocate (on the server) up to
      2^32 distinct fids (in theory) but this is obviously a bad thing to do.
      Instead clients are recommended to use fids from this (much smaller) list. *)

  include S.SERIALISABLE with type t := t
end

module OpenMode : sig
  type t =
  | Read       (** read access *)
  | Write      (** write access *)
  | ReadWrite  (** read and write access *)
  | Exec       (** execute access *)
  with sexp
  (** A 'mode' passed as an argument to "Open" and "Create" *)

  include S.SERIALISABLE with type t := t
end

module FileMode : sig
  type permission = [
    | `Read    (** subject has read access *)
    | `Write   (** subject has write access *)
    | `Execute (** subject may execute the file as a program *)
  ] with sexp

  type t = {
    owner: permission list; (** file owner has these permissions *)
    group: permission list; (** anyone in the same group has these permissions *)
    other: permission list; (** all other users have these permissions *)
    is_directory: bool;     (** true if the file is a directory *)
    append_only: bool;      (** true if the file is append-only (and therefore offsets in writes are ignored) *)
    exclusive: bool;        (** true if only one client may have it open at a time *)
    is_mount: bool;         (** true if the file is a mountpoint *)
    is_auth: bool;          (** true if the file is a special authentication file *)
    temporary: bool;        (** true if the file is temporary and should be skipped from nightly backups *)
    is_device: bool;        (** 9P2000.u: true if file is a char/block device *)
    is_symlink: bool;       (** 9P2000.u: true if file is a symlink *)
    is_namedpipe: bool;     (** 9P2000.u: true if file is a nomed pipe *)
    is_socket: bool;        (** 9P2000.u: true if file is a socket *)
    is_setuid: bool;        (** 9P2000.u: true if file is setuid *)
    is_setgid: bool;        (** 9P2000.u: true if file is setgid *)
  } with sexp
  (** A 'mode' returned from a call to "Stat" *)

  val make: ?owner:permission list -> ?group:permission list -> ?other:permission list ->
    ?is_directory:bool -> ?append_only:bool -> ?exclusive:bool -> ?is_mount:bool -> ?is_auth:bool -> ?temporary:bool ->
    ?is_device:bool -> ?is_symlink:bool -> ?is_namedpipe:bool -> ?is_socket:bool -> ?is_setuid:bool ->
    ?is_setgid:bool -> unit -> t

  include S.SERIALISABLE with type t := t
end

module Qid : sig
  type flag =
    | Directory  (** file is a directory *)
    | AppendOnly (** writes always hit the end of the file *)
    | Exclusive  (** file is opened for exclusive use *)
    | Mount      (** file is a mountpoint *)
    | Auth       (** file is an authentication file *)
    | Temporary  (** file is temporary and won't be backed up *)
    | Link       (** 9P2000.u: file is a symlink *)

  with sexp

  type t = {
   flags: flag list;
   version: int32;
   id: int64;
  } with sexp
  (** The server's unique id for the file. Two files are the same
      if and only if the Qids are the same. *)

  val file: ?id:int64 -> ?version:int32 -> ?append_only:bool -> ?exclusive:bool ->
    ?mount:bool -> ?auth:bool -> ?temporary:bool -> ?link:bool -> unit -> t
  (** Construct a [t] representing a file *)

  val dir: ?id:int64 -> ?version:int32 -> unit -> t
  (** Construct a [t] representing a directory *)

  include S.SERIALISABLE with type t := t
end

module Tag : sig
  type t with sexp

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t

  val notag: t
  (** The special tag value used during authentication *)

  val recommended: Set.t
  (** A list of recommended tags. The client can generate up to 2^16 distinct tags
      but this represents a large number of concurrent transactions. Instead clients
      are recommended to use fids drawn from this much (much smaller) list. *)

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
  type extension = {
    extension: string; (** 9P2000.u: extra information about links, pipes *)
    n_uid: int32;      (** 9P2000.u: numeric id of the user who owns the file *)
    n_gid: int32;      (** 9P2000.u: numeric id of the group of the file *)
    n_muid: int32;     (** 9P2000.u: numeric id of the user who last modified the file *)
  } with sexp

  val make_extension: ?extension:string -> ?n_uid:int32 -> ?n_gid:int32 -> ?n_muid:int32 -> unit -> extension

  type t = {
    ty: int;          (** for kernel use *)
    dev: int32;       (** for kernel use *)
    qid: Qid.t;
    mode: FileMode.t; (** permissions and flags *)
    atime: int32;     (** last access time *)
    mtime: int32;     (** last modification time *)
    length: int64;    (** length of the file in bytes *)
    name: string;     (** file name. Must be '/' if the file is the root *)
    uid: string;      (** owner name *)
    gid: string;      (** group name *)
    muid: string;     (** name of last user who modified the file *)
    u: extension option; (** 9P2000.u extensions *)
  } with sexp

  val make: name:string -> qid:Qid.t -> ?mode:FileMode.t -> ?length:int64 ->
    ?atime:int32 -> ?mtime:int32 -> ?uid:string -> ?gid:string -> ?muid:string ->
    ?u:extension -> unit -> t

  include S.SERIALISABLE with type t := t
end

module Arr(T: S.SERIALISABLE) : sig
  (** A sequence of [T.t]s written contiguously to a buffer *)

  include S.SERIALISABLE with type t = T.t list
end
