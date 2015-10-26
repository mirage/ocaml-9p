(*
 * Copyright (C) 2015 David Sheets <david.sheets@unikernel.com>
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

module type S = sig
  (** A traditional protocol message handler *)

  val walk: Server.info -> Request.Walk.t -> Response.payload Error.t Lwt.t

  val clunk: Server.info -> Request.Clunk.t -> Response.payload Error.t Lwt.t

  val open_: Server.info -> Request.Open.t -> Response.payload Error.t Lwt.t

  val read: Server.info -> Request.Read.t -> Response.payload Error.t Lwt.t

  val stat: Server.info -> Request.Stat.t -> Response.payload Error.t Lwt.t

  val create: Server.info -> Request.Create.t -> Response.payload Error.t Lwt.t

  val write: Server.info -> Request.Write.t -> Response.payload Error.t Lwt.t

  val remove: Server.info -> Request.Remove.t -> Response.payload Error.t Lwt.t
end
