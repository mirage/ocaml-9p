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

module type S = sig
  type t
  (** An established connection to a 9P server *)

  val after_disconnect: t -> unit Lwt.t
  (** A thread which wakes up when the connection to the server
      has been broken *)

  val disconnect: t -> unit Lwt.t
  (** Disconnect from the 9P server, but leave the underlying FLOW
      connected. *)

  val create: t -> string list -> string ->
    Protocol_9p_types.FileMode.t -> unit Protocol_9p_error.t Lwt.t
  (** [create t path name perm] creates a new empty file [name] inside
      [path] with * the given permissions. *)

  val write: t -> string list -> int64 -> Cstruct.t ->
    unit Protocol_9p_error.t Lwt.t
  (** [write t path offset buf] writes [buf] to the file at [path] at
      offset [offset] *)

  val read: t -> string list -> int64 -> int32 ->
    Cstruct.t list Protocol_9p_error.t Lwt.t
  (** [read t path offset count] returns a list of buffers containing [count]
      bytes from offset [offset] in the file given by [path] *)

  val mkdir: t -> string list -> string ->
    Protocol_9p_types.FileMode.t -> unit Protocol_9p_error.t Lwt.t
  (** [mkdir t path name perm] creates a new directory [name] inside
      [path] with * the given permissions. *)

  val remove: t -> string list -> unit Protocol_9p_error.t Lwt.t
  (** [remove t path] removes a file or directory from the filesystem. *)

  val readdir: t -> string list ->
    Protocol_9p_types.Stat.t list Protocol_9p_error.t Lwt.t
  (** Return the contents of a named directory. *)

  val stat: t -> string list ->
    Protocol_9p_types.Stat.t Protocol_9p_error.t Lwt.t
  (** Return information about a named directory or named file. *)

  module KV_RO : Mirage_kv_lwt.RO with type t = t

  module LowLevel : sig
    (** The functions in this module are mapped directly onto individual 9P
        RPCs. The client must carefully respect the rules on managing fids
        and stay within the message size limits. *)

    val maximum_write_payload: t -> int32
    (** The largest payload that can be written in one go. *)

    val allocate_fid: t -> Protocol_9p_types.Fid.t Protocol_9p_error.t Lwt.t
    (** [allocate_fid t] returns a free fid. Callers must call [deallocate_fid t]
        when they are finished with it. *)

    val deallocate_fid: t -> Protocol_9p_types.Fid.t -> unit Lwt.t
    (** [deallocate_fid t fid] clunks a fid and marks it as free for re-use. *)

    val walk: t -> Protocol_9p_types.Fid.t -> Protocol_9p_types.Fid.t ->
      string list -> Protocol_9p_response.Walk.t Protocol_9p_error.t Lwt.t
    (** [walk t fid newfid wnames] binds [newfid] to the result of Walking
        from [fid] along the path given by [wnames] *)

    val openfid: t -> Protocol_9p_types.Fid.t ->
      Protocol_9p_types.OpenMode.t ->
      Protocol_9p_response.Open.t Protocol_9p_error.t Lwt.t
    (** [open t fid mode] confirms that [fid] can be accessed according to
        [mode] *)

    val create: t -> Protocol_9p_types.Fid.t -> ?extension:string -> string ->
      Protocol_9p_types.FileMode.t ->
      Protocol_9p_types.OpenMode.t ->
      Protocol_9p_response.Create.t Protocol_9p_error.t Lwt.t
    (** [create t fid name perm mode] creates a new file or directory
        called [name] and with permissions [perm] inside the directory
        [fid] and opens it according to [mode] (which is not checked
        against [perm]). *)

    val stat: t -> Protocol_9p_types.Fid.t ->
      Protocol_9p_response.Stat.t Protocol_9p_error.t Lwt.t
    (** [stat t fid] returns a description of the file associated with [fid] *)

    val wstat: t -> Protocol_9p_types.Fid.t -> Protocol_9p_types.Stat.t ->
      Protocol_9p_response.Wstat.t Protocol_9p_error.t Lwt.t
    (** [wstat t fid stat] changes the file metadata according to [stat]. *)

    val read: t -> Protocol_9p_types.Fid.t -> int64 -> int32 ->
      Protocol_9p_response.Read.t Protocol_9p_error.t Lwt.t
    (** [read t fid offset count] returns [count] bytes of data at [offset] in
        the file referenced by [pid]. Note that [count] must be less than the
        server's negotiated maximum message size. *)

    val write: t -> Protocol_9p_types.Fid.t -> int64 -> Cstruct.t ->
      Protocol_9p_response.Write.t Protocol_9p_error.t Lwt.t
    (** [write t fid offset data] writes [data] to the file given by [fid] at
        offset [offset]. [data] must not exceed [maximum_write_payload t]. *)

    val clunk: t -> Protocol_9p_types.Fid.t ->
      Protocol_9p_response.Clunk.t Protocol_9p_error.t Lwt.t
    (** [clunk t fid] informs the server that the reference [fid] should be
        forgotten about. When this call returns, it is safe for the client to
        re-use the fid. *)

    val remove: t -> Protocol_9p_types.Fid.t ->
      Protocol_9p_response.Remove.t Protocol_9p_error.t Lwt.t
    (** [remove t fid] removes the file associated with [fid] from the file
        server. The server will "clunk" the fid whether the call succeeds or
        fails. *)

    val update: t ->
       ?name:string ->
       ?length:int64 ->
       ?mode:Protocol_9p_types.FileMode.t ->
       ?mtime:int32 ->
       ?gid:string ->
       Protocol_9p_types.Fid.t -> unit Protocol_9p_error.t Lwt.t
    (** Convenience wrapper around [wstat]. *)
  end

  val walk_from_root: t -> Protocol_9p_types.Fid.t -> string list ->
    Protocol_9p_response.Walk.t Protocol_9p_error.t Lwt.t
  (** [walk_from_root t] is [LowLevel.walk t root], where [root] is
      the internal Fid representing the root directory (which is not
      exposed by the API). *)

  val with_fid: t -> (Protocol_9p_types.Fid.t -> 'a Protocol_9p_error.t Lwt.t)
      -> 'a Protocol_9p_error.t Lwt.t
  (** [with_fid t fn] is the result of running [fn x] with a fresh Fid
      [x], which is returned to the free pool when the thread
      finishes. *)
end

(** Given a transport (a Mirage FLOW), construct a 9P client on top. *)
module Make(Log: Protocol_9p_s.LOG)(FLOW: Mirage_flow_lwt.S) : sig
  include S

  val connect:
    FLOW.flow -> ?msize:int32 -> ?username:string -> ?max_fids:int32 ->
    ?aname:string -> unit ->
    t Protocol_9p_error.t Lwt.t
  (** Establish a fresh connection to a 9P server. [msize] gives the maximum
      message size we support: the server may choose a lower value. [username]
      is the username to present to the remote server. [max_fids] is the default
      number of maximum openened fids: by default it is set to [100].
      [aname] is the name of the exported filesystem. *)

end
