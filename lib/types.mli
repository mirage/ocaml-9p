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

val big_enough_for: string -> Cstruct.t -> int -> unit Error.t
(** [big_enough_for name buf length] returns an error with a log message
    if buffer [buf] is smaller than [length]. The [name] will be included
    in the error message. *)

module Int16 : sig
  type t = int

  include S.SERIALISABLE with type t := t
end

module Int32 : sig
  type t = int32

  include S.SERIALISABLE with type t := t
end

module Qid : sig
  type t = string (** 13 bytes long *)

  include S.SERIALISABLE with type t := t
end

module Data : sig
  type t = Cstruct.t
  (** A length-prefixed chunk of data which may include embedded NULLs, or may be
      interpreted later as UTF-8 text. *)

  val of_string: string -> t

  val to_string: t -> string

  include S.SERIALISABLE with type t := t
end
