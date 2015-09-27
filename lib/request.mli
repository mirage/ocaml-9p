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
open Result

module Version : sig

  type t = {
    msize: int32;
    version: string;
  }
  (** The payload of a version message *)

  include S.SERIALISABLE with type t := t
end

module Auth : sig

  type t = {
    afid: int32;
    uname: string;
    aname: string;
  }
  (** The payload of a version message *)

  include S.SERIALISABLE with type t := t
end

module Flush : sig

  type t = {
    oldtag: int;
  }
  (** The payload of a flush message *)

  include S.SERIALISABLE with type t := t
end

module Attach : sig

  type t = {
    fid: int32;
    afid: int32;
    uname: string;
    aname: string;
  }
  (** The payload of an attach message *)

  include S.SERIALISABLE with type t := t
end

type payload =
  | Version of Version.t
  | Auth of Auth.t
  | Flush of Flush.t
  | Attach of Attach.t

type t = {
  tag: int;
  payload: payload;
}

val sizeof: t -> int