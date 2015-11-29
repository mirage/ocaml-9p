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

open Result

type 'a or_error = ('a, Response.Err.t) result

module type S = sig
  (** A traditional protocol message handler.
     If an [Error] is returned, it will be reported back to the client. *)

  val attach: Server.info -> cancel:unit Lwt.t -> Request.Attach.t -> Response.Attach.t or_error Lwt.t
  val walk: Server.info -> cancel:unit Lwt.t -> Request.Walk.t -> Response.Walk.t or_error Lwt.t
  val clunk: Server.info -> cancel:unit Lwt.t -> Request.Clunk.t -> Response.Clunk.t or_error Lwt.t
  val open_: Server.info -> cancel:unit Lwt.t -> Request.Open.t -> Response.Open.t or_error Lwt.t
  val read: Server.info -> cancel:unit Lwt.t -> Request.Read.t -> Response.Read.t or_error Lwt.t
  val stat: Server.info -> cancel:unit Lwt.t -> Request.Stat.t -> Response.Stat.t or_error Lwt.t
  val create: Server.info -> cancel:unit Lwt.t -> Request.Create.t -> Response.Create.t or_error Lwt.t
  val write: Server.info -> cancel:unit Lwt.t -> Request.Write.t -> Response.Write.t or_error Lwt.t
  val remove: Server.info -> cancel:unit Lwt.t -> Request.Remove.t -> Response.Remove.t or_error Lwt.t
  val wstat: Server.info -> cancel:unit Lwt.t -> Request.Wstat.t -> Response.Wstat.t or_error Lwt.t
end
