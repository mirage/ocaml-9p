(*
 * Copyright (C) 2015 David Scott <dave.scott@unikernel.com>
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
open Protocol_9p
open Lwt

module Make(Log : S.LOG) : sig
  type t

  type ip = string
  type port = int

  val create: ip -> port -> Server.receive_cb -> t
  (** [create ip port receive_cb] creates a stopped server instance
      which will -- when started -- listen on [ip:port] and serve 9P
      requests through the [receive_cb] callback. *)

  val shutdown: t -> unit Lwt.t
  (** [shutdown t] requests that the running server [t] is shutdown,
      and blocks until the shutdown is complete and resources are freed. *)

  val serve_forever: ?listening:(unit Lwt.u) -> t -> unit Lwt.t
  (** [serve_forever ?listening t] starts serving 9P requests via [t].
      The wakener [?listening] will be woken with [()] when the local ports
      have been bound and it's safe to call [connect] *)
end
