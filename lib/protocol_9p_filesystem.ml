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

type 'a or_error = ('a, Protocol_9p_response.Err.t) result

module type S = sig

  type t
  type connection

  val connect: t -> Protocol_9p_info.t -> connection
  val disconnect: connection -> Protocol_9p_info.t -> unit Lwt.t

  val attach:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Attach.t ->
    Protocol_9p_response.Attach.t or_error Lwt.t

  val walk:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Walk.t -> Protocol_9p_response.Walk.t or_error Lwt.t

  val clunk:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Clunk.t -> Protocol_9p_response.Clunk.t or_error Lwt.t

  val open_:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Open.t -> Protocol_9p_response.Open.t or_error Lwt.t

  val read:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Read.t -> Protocol_9p_response.Read.t or_error Lwt.t

  val stat:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Stat.t -> Protocol_9p_response.Stat.t or_error Lwt.t

  val create:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Create.t ->
    Protocol_9p_response.Create.t or_error Lwt.t

  val write:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Write.t -> Protocol_9p_response.Write.t or_error Lwt.t

  val remove:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Remove.t ->
    Protocol_9p_response.Remove.t or_error Lwt.t

  val wstat:
    connection ->
    cancel:unit Lwt.t ->
    Protocol_9p_request.Wstat.t -> Protocol_9p_response.Wstat.t or_error Lwt.t
end
