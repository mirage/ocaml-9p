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

(** Given a transport (a Mirage FLOW), construct a 9P server on top. *)

type exn_converter = Protocol_9p_info.t -> exn -> Protocol_9p_response.payload
(**
An exception converter transforms the given OCaml exception into a 9P error
response. 9P clients like Linux work better if they receive 9P2000.u errno
values which happen to match their local errno definition.

Here is an example converter for Linux and OS X hosts:
{[
let unix_exn_converter info exn =
  let is_unix = (info.Protocol_9p_info.version = Types.Version.unix) in
  match exn with
  | Unix.Unix_error(err, _, _) ->
    let host = match info.Protocol_9p_info.aname with
      | "linux#/" when is_unix -> Some Errno_host.Linux.v4_0_5
      | "osx#/" when is_unix -> Some Errno_host.OSX.v10_11_1
      | _ -> None
    in
    let errno = match host with
      | None -> None
      | Some host -> match Errno_unix.of_unix ~host err with
        | [] -> None
        | errno::_ -> match Errno.to_code ~host errno with
          | None -> None
          | Some i -> Some (Int32.of_int i)
    in
    Response.Err {
      Response.Err.ename = Unix.error_message err;
      errno;
    }
  | e ->
    Response.Err {
      Response.Err.ename = Printexc.to_string e;
      errno = None;
    }
]}
*)


module Make
    (Log: Protocol_9p_s.LOG)
    (FLOW: Mirage_flow.S)
    (Filesystem: Protocol_9p_filesystem.S) :
sig

  type t
  (** An established connection to a 9P client *)

  val connect: Filesystem.t -> FLOW.flow -> ?msize:int32
    -> ?exn_converter:exn_converter -> unit
    -> t Protocol_9p_error.t Lwt.t
  (** Establish a fresh connection to a 9P client. [msize] gives the maximum
      message size we support: the client may request a lower value.
      [receive_cb] will be called with every 9P request. *)

  val get_info: t -> Protocol_9p_info.t
  (** Return information about the current connection *)

  val disconnect: t -> unit Lwt.t
  (** [disconnect connection] causes the connection [connection] to
      close after the next message is processed. Once the connection
      has been disconnected, the returned thread will resolve. *)

  val after_disconnect: t -> unit Lwt.t
  (** [after_disconnect connection] resolves after [connection] has
      disconnected. *)
end
