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

(** Parsers and printers for 9P response messages *)

open Protocol_9p_types

module Version : sig

  type t = {
    msize: int32; (** the server's maximum message size, must be less than
                      the client's *)
    version: Protocol_9p_types.Version.t;
  } [@@deriving sexp]
  (** The payload of a version message *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Auth : sig

  type t = {
    aqid: Qid.t;
  } [@@deriving sexp]
  (** The payload of an authentication response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Err : sig

  type t = {
    ename: string;
    errno: int32 option; (** The extended 9P2000.u protocol allows the server
                             to return an errno *)
  } [@@deriving sexp]
  (** The pauload of an error response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Flush : sig
  type t = unit [@@deriving sexp]
  (** The payload of a flush response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Attach : sig
  type t = {
    qid: Qid.t
  } [@@deriving sexp]
  (** The payload of an attach response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Walk : sig
  type t = {
    wqids: Qid.t list
  } [@@deriving sexp]
  (** The payload of a walk response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Open : sig
  type t = {
    qid: Qid.t;
    iounit: int32;
  } [@@deriving sexp]
  (** The payload of an Open response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Create : sig
  type t = {
    qid: Qid.t;
    iounit: int32;
  } [@@deriving sexp]
  (** The payload of a Create response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Read : sig
  type t = {
    data: Cstruct.t
  } [@@deriving sexp]
  (** The payload of a Read response *)

  val sizeof_header: int
  (** The size of only the header *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Write : sig
  type t = {
    count: int32
  } [@@deriving sexp]
  (** The payload of a Write response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Clunk : sig
  type t = unit [@@deriving sexp]
  (** The payload of a Clunk response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Remove : sig
  type t = unit [@@deriving sexp]
  (** The payload of a Remove response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Stat : sig
  type t = {
    stat: Stat.t;
  } [@@deriving sexp]
  (** The payload of a Stat response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

module Wstat : sig
  type t = unit [@@deriving sexp]
  (** The payload of a Wstat response *)

  include Protocol_9p_s.SERIALISABLE with type t := t
end

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
[@@deriving sexp]
(** A variant including all possible 9P response payloads *)

type t = {
  tag: Protocol_9p_types.Tag.t; (** The tag used to match this response with
                                    the original request *)
  payload: payload;
} [@@deriving sexp]
(** A 9P protocol response *)

include Protocol_9p_s.SERIALISABLE with type t := t

val pp: t Fmt.t
(** [pp] pretty-prints responses. *)

val equal: t -> t -> bool
(** [equal] is the equality function over responses. *)

val sizeof_header: int
(** The size of the fixed response header *)

val error: ?errno:int32 -> ('a, unit, string, (_, Err.t) result) format4 -> 'a
