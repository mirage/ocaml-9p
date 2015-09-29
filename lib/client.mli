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

(** Given a transport (a Mirage FLOW), construct a 9P client on top. *)

module Make(Log: S.LOG)(FLOW: V1_LWT.FLOW) : sig

  type t
  (** An established connection to a 9P server *)

  val connect: FLOW.flow -> ?msize:int32 -> ?username:string -> ?aname:string -> unit -> t Error.t Lwt.t
  (** Establish a fresh connection to a 9P server. [msize] gives the maximum
      message size we support: the server may choose a lower value. [username]
      is the username to present to the remote server. [aname] is the name of
      the exported filesystem. *)

  val disconnect: t -> unit Lwt.t
  (** Disconnect from the 9P server, but leave the underlying FLOW connected. *)

  val read: t -> string list -> int64 -> int32 -> Cstruct.t list Error.t Lwt.t
  (** [read t path offset count] returns a list of buffers containing [count]
      bytes from offset [offset] in the file given by [path] *)

  val readdir: t -> string list -> Types.Stat.t list Error.t Lwt.t
  (** Return the contents of a named directory. *)

  val stat: t -> string list -> Types.Stat.t Error.t Lwt.t
  (** Return information about a named directory or named file. *)

  module KV_RO : V1_LWT.KV_RO

  module LowLevel : sig
    (** The functions in this module are mapped directly onto individual 9P
        RPCs. The client must carefully respect the rules on managing fids
        and stay within the message size limits. *)

    val walk: t -> Types.Fid.t -> Types.Fid.t -> string list -> Response.Walk.t Error.t Lwt.t
    (** [walk t fid newfid wnames] binds [newfid] to the result of Walking
        from [fid] along the path given by [wnames] *)

    val openfid: t -> Types.Fid.t -> Types.OpenMode.t -> Response.Open.t Error.t Lwt.t
    (** [open t fid mode] confirms that [fid] can be accessed according to
        [mode] *)

    val stat: t -> Types.Fid.t -> Response.Stat.t Error.t Lwt.t
    (** [stat t fid] returns a description of the file associated with [fid] *)

    val read: t -> Types.Fid.t -> int64 -> int32 -> Response.Read.t Error.t Lwt.t
    (** [read t fid offset count] returns [count] bytes of data at [offset] in
        the file referenced by [pid]. Note that [count] must be less than the
        server's negotiated maximum message size. *)

    val write: t -> Types.Fid.t -> int64 -> Cstruct.t -> Response.Write.t Error.t Lwt.t
    (** [write t fid offset data] writes [data] to the file given by [fid] at
        offset [offset]. *)

    val clunk: t -> Types.Fid.t -> Response.Clunk.t Error.t Lwt.t
    (** [clunk t fid] informs the server that the reference [fid] should be
        forgotten about. When this call returns, it is safe for the client to
        re-use the fid. *)

    val remove: t -> Types.Fid.t -> Response.Remove.t Error.t Lwt.t
    (** [remove t fid] removes the file associated with [fid] from the file
        server. The server will "clunk" the fid whether the call succeeds or
        fails. *)
  end
end
