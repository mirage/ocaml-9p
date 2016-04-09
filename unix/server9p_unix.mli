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

module Make(Log : S.LOG)(Filesystem: Filesystem.S) : sig
  type t

  val of_fd: Filesystem.t -> Lwt_unix.file_descr -> t
  (** [of_fd fs fd] returns a server configured to accept connections on
      listening socket [fd]. *)

  val listen: Filesystem.t -> string -> string -> t Error.t Lwt.t
  (** [listen fs proto address callback] listens on the address [address] and prepares
      to serve 9P via the [callback]. The [proto] and [address] can be either:
      - unix /path/to/socket
      - tcp ip:port *)

  val shutdown: t -> unit Lwt.t
  (** [shutdown t] requests that the running server [t] is shutdown,
      and blocks until the shutdown is complete and resources are freed. *)

  val serve_forever: t -> unit Error.t Lwt.t
  (** [serve_forever t] starts serving 9P requests via [t]. This thread
      only returns when the server has been shutdown. *)
end
