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

open Protocol_9p_infix

module Error = Protocol_9p_error
open Error

module Types = Protocol_9p_types
module Request = Protocol_9p_request
module Response = Protocol_9p_response

module type S = sig
  type t

  val after_disconnect: t -> unit Lwt.t

  val disconnect: t -> unit Lwt.t

  val create: t -> string list -> string -> Types.FileMode.t -> unit Error.t Lwt.t
  val write: t -> string list -> int64 -> Cstruct.t -> unit Protocol_9p_error.t Lwt.t
  val read: t -> string list -> int64 -> int32 -> Cstruct.t list Error.t Lwt.t
  val mkdir: t -> string list -> string -> Types.FileMode.t -> unit Error.t Lwt.t
  val remove: t -> string list -> unit Error.t Lwt.t
  val readdir: t -> string list -> Types.Stat.t list Error.t Lwt.t
  val stat: t -> string list -> Types.Stat.t Error.t Lwt.t

  module LowLevel : sig
    val maximum_write_payload: t -> int32
    val allocate_fid: t -> Protocol_9p_types.Fid.t Error.t Lwt.t
    val deallocate_fid: t -> Protocol_9p_types.Fid.t -> unit Lwt.t
    val walk: t -> Types.Fid.t -> Types.Fid.t -> string list -> Response.Walk.t Error.t Lwt.t
    val openfid: t -> Types.Fid.t -> Types.OpenMode.t -> Response.Open.t Error.t Lwt.t
    val create: t -> Types.Fid.t -> ?extension:string -> string -> Types.FileMode.t ->
                Types.OpenMode.t -> Response.Create.t Error.t Lwt.t
    val stat: t -> Types.Fid.t -> Response.Stat.t Error.t Lwt.t
    val wstat: t -> Types.Fid.t -> Types.Stat.t -> Response.Wstat.t Error.t Lwt.t
    val read: t -> Types.Fid.t -> int64 -> int32 -> Response.Read.t Error.t Lwt.t
    val write: t -> Types.Fid.t -> int64 -> Cstruct.t -> Response.Write.t Error.t Lwt.t
    val clunk: t -> Types.Fid.t -> Response.Clunk.t Error.t Lwt.t
    val remove: t -> Types.Fid.t -> Response.Remove.t Error.t Lwt.t

    val update: t -> ?name:string -> ?length:int64 -> ?mode:Types.FileMode.t ->
                ?mtime:int32 -> ?gid:string -> Types.Fid.t -> unit Error.t Lwt.t
  end

  val walk_from_root: t -> Types.Fid.t -> string list -> Response.Walk.t Error.t Lwt.t
  val with_fid: t -> (Types.Fid.t -> 'a Error.t Lwt.t) -> 'a Error.t Lwt.t
end

module Make(Log: Protocol_9p_s.LOG)(FLOW: Mirage_flow.S) = struct
  module Reader = Protocol_9p_buffered9PReader.Make(Log)(FLOW)

  open Log

  type fid = Types.Fid.t

  type t = {
    reader: Reader.t;
    writer: FLOW.flow;
    root: fid;
    msize: int32;
    upgraded: bool; (* 9P2000.u *)
    maximum_payload: int32;
    transmit_m: Lwt_mutex.t;
    mutable please_shutdown: bool;
    shutdown_m: Lwt_mutex.t;
    shutdown_complete_t: unit Lwt.t;
    mutable wakeners: Response.payload Error.t Lwt.u Types.Tag.Map.t;
    mutable free_tags: Types.Tag.Set.t;
    free_tags_c: unit Lwt_condition.t;
    max_fids: int32;
    mutable fids: Types.Fid.Set.t;
    free_fids_c: unit Lwt_condition.t;
  }

  (* For converting flow errors *)
  let (>>|=) m f =
    let open Lwt in
    m >>= function
    | Ok x -> f x
    | Error `Closed -> return (error_msg "Writing to closed FLOW")
    | Error e       ->
      return (error_msg "Unexpected error on underlying FLOW: %a"
                FLOW.pp_write_error e)

  let read_one_packet reader =
    Reader.read reader
    >>*= fun buffer ->
    Lwt.return (Response.read buffer)
    >>*= fun (response, _) ->
    debug (fun f -> f "S %a" Response.pp response);
    Lwt.return (Ok response)

  let write_one_packet flow request =
    debug (fun f -> f "C %a" Request.pp request);
    let sizeof = Request.sizeof request in
    let buffer = Cstruct.create sizeof in
    Lwt.return (Request.write request buffer)
    >>*= fun _ ->
    FLOW.write flow buffer
    >>|= fun () ->
    Lwt.return (Ok ())

  let finally f g =
    let open Lwt in
    Lwt.catch
      (fun () ->
        f ()
        >>= fun ok_or_error ->
        g ()
        >>= fun _ignore_error ->
        return ok_or_error
      ) (fun e ->
        g ()
        >>= fun _ignore_error ->
        fail e)

  let dispatcher_is_running t = Lwt.state t.shutdown_complete_t = Lwt.Sleep

  let rpc t request : Response.payload Error.t Lwt.t =
    (* Allocate a fresh tag, or wait if none are available yet *)
    let rec allocate_tag () =
      let open Lwt in
      if t.free_tags = Types.Tag.Set.empty && (dispatcher_is_running t)
      then Lwt_condition.wait t.free_tags_c >>= fun () -> allocate_tag ()
      else
        if not(dispatcher_is_running t)
        then return (Error (`Msg "connection disconnected"))
        else
        let tag = Types.Tag.Set.min_elt t.free_tags in
        t.free_tags <- Types.Tag.Set.remove tag t.free_tags;
        let th, wakener = Lwt.task () in
        t.wakeners <- Types.Tag.Map.add tag wakener t.wakeners;
        return (Ok (tag, th)) in
    let deallocate_tag tag =
      t.free_tags <- Types.Tag.Set.add tag t.free_tags;
      (* The tag will have already been removed from the wakeners map *)
      Lwt_condition.signal t.free_tags_c ();
      Lwt.return () in
    let with_tag f =
      allocate_tag ()
      >>*= fun (tag, th) ->
      finally (fun () -> f (tag, th)) (fun () -> deallocate_tag tag) in
    with_tag
      (fun (tag, th) ->
        (* Lock the flow for output and transmit the packet *)
        Lwt_mutex.with_lock t.transmit_m
          (fun () ->
            let request = { Request.tag; payload = request } in
            write_one_packet t.writer request
          )
        >>*= fun () ->
        (* Wait for the response (or error) to be read *)
        th
      )

  (* The dispatcher thread reads responses from the FLOW and wakes up
     the thread blocked in the rpc function. *)
  let rec dispatcher_t shutdown_complete_wakener t =
    if t.please_shutdown then begin
      Lwt.return (Ok ())
    end else read_one_packet t.reader
    >>*= fun response ->
    let tag = response.Response.tag in
    if not(Types.Tag.Map.mem tag t.wakeners) then begin
      let pretty_printed = Sexplib.Sexp.to_string (Response.sexp_of_t response) in
      err (fun f -> f "Received response with unexpected tag: %s" pretty_printed);
      dispatcher_t shutdown_complete_wakener t
    end else begin
      let wakener = Types.Tag.Map.find tag t.wakeners in
      Lwt.wakeup_later wakener (Ok response.Response.payload);
      t.wakeners <- Types.Tag.Map.remove tag t.wakeners;
      dispatcher_t shutdown_complete_wakener t
    end

  let return_error = function
    | Response.Err { Response.Err.ename; _ } ->
      Lwt.return (Error (`Msg ename))
    | payload ->
      Lwt.return (error_msg "Server sent unexpected reply: %s" (Sexplib.Sexp.to_string (Response.sexp_of_payload  payload)))

  module LowLevel = struct
    let maximum_write_payload t = t.maximum_payload

    let flush t oldtag =
      rpc t Request.(Flush { Flush.oldtag })
      >>*= function
      | Response.Flush x -> Lwt.return (Ok x)
      | response -> return_error response

    let walk t fid newfid wnames =
      rpc t Request.(Walk { Walk.fid; newfid; wnames })
      >>*= function
      | Response.Walk x -> Lwt.return (Ok x)
      | response -> return_error response

    let create t fid ?extension name perm mode =
      rpc t Request.(Create { Create.fid; name; perm; mode; extension })
      >>*= function
      | Response.Create x -> Lwt.return (Ok x)
      | response -> return_error response

    let openfid t fid mode =
      rpc t Request.(Open { Open.fid; mode })
      >>*= function
      | Response.Open x -> Lwt.return (Ok x)
      | response -> return_error response

    let stat t fid =
      rpc t Request.(Stat { Stat.fid })
      >>*= function
      | Response.Stat x -> Lwt.return (Ok x)
      | response -> return_error response

    let read t fid offset count =
      rpc t Request.(Read { Read.fid; offset; count })
      >>*= function
      | Response.Read x -> Lwt.return (Ok x)
      | response -> return_error response

    let write t fid offset data =
      rpc t Request.(Write { Write.fid; offset; data })
      >>*= function
      | Response.Write x -> Lwt.return (Ok x)
      | response -> return_error response

    let clunk t fid =
      rpc t Request.(Clunk { Clunk.fid })
      >>*= function
      | Response.Clunk x -> Lwt.return (Ok x)
      | response -> return_error response

    let remove t fid =
      rpc t Request.(Remove { Remove.fid })
      >>*= function
      | Response.Remove x -> Lwt.return (Ok x)
      | response -> return_error response

    let wstat t fid stat =
      rpc t Request.(Wstat { Wstat.fid; stat })
      >>*= function
      | Response.Wstat x -> Lwt.return (Ok x)
      | response -> return_error response

    let update t ?(name="") ?(length=Types.Int64.any) ?(mode=Types.FileMode.any)
                 ?(mtime=Types.Int32.any) ?(gid="") fid =
      wstat t fid { Types.Stat.
        name;
        length;
        mode;
        mtime;
        gid;
        (* It's illegal to set these *)
        ty = Types.Int16.any;
        dev = Types.Int32.any;
        qid = Types.Qid.any;
        atime = Types.Int32.any;
        uid = "";
        muid = "";
        u = None;
      }

    let version reader writer msize version =
      write_one_packet writer {
        Request.tag = Types.Tag.notag;
        payload = Request.Version Request.Version.({ msize; version });
      } >>*= fun () ->
      read_one_packet reader
      >>*= fun response ->
      match response with
      | { Response.payload = Response.Version v; _ } ->
        Lwt.return (Ok v)
      | { Response.payload = p; _ } -> return_error p

    let attach reader writer fid afid uname aname n_uname =
      let tag = Types.Tag.Set.min_elt Types.Tag.recommended in
      write_one_packet writer {
        Request.tag;
        payload = Request.Attach Request.Attach.({ fid; afid; uname; aname; n_uname })
      } >>*= fun () ->
      read_one_packet reader
      >>*= fun response ->
      match response with
      | { Response.payload = Response.Attach x; _ } ->
        Lwt.return (Ok x)
      | { Response.payload = p; _ } -> return_error p

    let fid = function
      | 0l -> (* 0 is the pre-allocated FS root *) None
      | n  ->
        match Types.Fid.of_int32 n with
        | Ok m    -> Some m
        | Error _ -> (* NOFID *) None

    let rec random_fid ?(n=10) t =
      if n = 0 then (
        Log.info (fun l -> l "Cannot allocate a new random fid after 10 tries");
        None
      ) else match fid @@ Random.int32 Int32.max_int with
        | None        -> random_fid ~n:(n-1) t
        | Some f as r -> if Types.Fid.Set.mem f t.fids then random_fid t else r

    let min_fid t =
      match fid @@ Int32.pred Types.Fid.(to_int32 @@ Set.min_elt t.fids) with
      | None        -> random_fid t
      | Some _ as r -> r

    (* if max_fids is not reached, the allocation strategy is:
       - pick max(allocated_fid) + 1
       - if this is NOFID:
         - pick min(allocated_fid) - 1
         - if this is 0 or NOFID:
           - pick a random fid until finding a non-allocated one
       This means that keeping [0=(NOFID+1)] and [NOFID-1] always open
       might be costly.*)
    let next_fid t =
      match Int32.of_int (Types.Fid.Set.cardinal t.fids) with
      | 0l -> fid 1l (* 0l is pre-allocated for the FS root *)
      | n  ->
        if n >= t.max_fids then None else
          Types.Fid.Set.max_elt t.fids
          |> Types.Fid.to_int32
          |> Int32.succ
          |> fid
          |> function
          | Some _ as r -> r
          | None        -> min_fid t

    let rec allocate_fid t =
      let open Lwt.Infix in
      match next_fid t with
      | None ->
        if dispatcher_is_running t then (
          Log.info (fun f -> f "FID pool exhausted (will wait for a free one; \
                                deadlock possible)");
          Lwt_condition.wait t.free_fids_c >>= fun () -> allocate_fid t
        ) else
          Lwt.return (Error (`Msg "connection disconnected"))
      | Some fid ->
        t.fids <- Types.Fid.Set.add fid t.fids;
        Lwt.return (Ok fid)
    let mark_fid_as_free t fid =
      t.fids <- Types.Fid.Set.remove fid t.fids;
      Lwt_condition.signal t.free_fids_c ()
    let deallocate_fid t fid =
      let open Lwt in
      clunk t fid
      >>= fun _ -> (* the spec says to assume the fid is clunked now *)
      mark_fid_as_free t fid;
      Lwt.return ()
  end

  let walk_from_root t = LowLevel.walk t t.root

  let with_fid t f =
    LowLevel.allocate_fid t
    >>*= fun fid ->
    finally (fun () -> f fid) (fun () -> LowLevel.deallocate_fid t fid)

  let with_walk_from_root t path f =
    let open Lwt in
    LowLevel.allocate_fid t
    >>*= fun newfid ->
    LowLevel.walk t t.root newfid path
    >>= function
    | Error e ->
      (* No need to clunk the fid as it's not bound *)
      LowLevel.mark_fid_as_free t newfid;
      Lwt.return (Error e)
    | Ok _ -> (* I don't need to know the qids *)
      finally (fun () -> f newfid) (fun () -> LowLevel.deallocate_fid t newfid)

  let write t path offset buf =
    let open LowLevel in
    with_walk_from_root t path
      (fun newfid ->
        openfid t newfid Types.OpenMode.write_only
        >>*= fun _ ->
        let rec loop offset remaining =
          let len = Cstruct.len remaining in
          if len = 0
          then Lwt.return (Ok ())
          else begin
            let to_request = min len (Int32.to_int t.maximum_payload) in
            write t newfid offset (Cstruct.sub remaining 0 to_request)
            >>*= fun { Response.Write.count } ->
            let count = Int32.to_int count in
            let remaining = Cstruct.shift remaining count in
            loop Int64.(add offset (of_int count)) remaining
          end in
        loop offset buf
      )

  let read t path offset count =
    let open LowLevel in
    with_walk_from_root t path
      (fun newfid ->
        openfid t newfid Types.OpenMode.read_only
        >>*= fun _ ->
        let rec loop acc offset remaining =
          let to_request = min remaining t.maximum_payload in
          read t newfid offset to_request
          >>*= fun { Response.Read.data } ->
          let n = Cstruct.len data in
          if n = 0
          then Lwt.return (Ok (List.rev acc))
          else
            loop (data :: acc) Int64.(add offset (of_int n)) Int32.(sub remaining (of_int n)) in
         loop [] offset count
      )

  let create t path name perm =
    let open LowLevel in
    with_walk_from_root t path
      (fun newfid ->
        create t newfid name perm Types.OpenMode.read_only
        >>*= fun _ ->
        Lwt.return (Ok ())
      )

  let mkdir t path name perm = create t path name {perm with Types.FileMode.is_directory = true}

  let remove t path =
    let open LowLevel in
    let open Lwt.Infix in
    let fid = t.root in
    LowLevel.allocate_fid t
    >>*= fun newfid ->
    walk t fid newfid path
    >>= function
    | Error e ->
      (* We must clunk the fid ourselves *)
      clunk t newfid
      >>= fun _ -> (* ignore cascade error *)
      mark_fid_as_free t newfid;
      Lwt.return (Error e)
    | Ok _ ->
      remove t newfid
      >>= fun result ->
      (* Fid has been clunked by the remove call even on failure *)
      mark_fid_as_free t newfid;
      Lwt.return result

  let stat t path =
    let open LowLevel in
    with_walk_from_root t path
      (fun newfid ->
        stat t newfid
        >>*= fun { Response.Stat.stat } ->
        Lwt.return (Ok stat)
      )

  let after_disconnect t = t.shutdown_complete_t

  let disconnect t =
    let open Lwt in
    Lwt_mutex.with_lock t.shutdown_m
      (fun () ->
        if dispatcher_is_running t then begin
          (* Mark the connection as shutting down, so the dispatcher will quit *)
          t.please_shutdown <- true;
          (* Send a request, to unblock the dispatcher *)
          LowLevel.flush t Types.Tag.notag
          >>= fun _ ->
          (* Wait for the dispatcher to shutdown *)
          t.shutdown_complete_t
          >>= fun () ->
          (* Any new callers of `rpc` will fail immediately without blocking. *)
          return ()
        end else
          return ()
      )

  let readdir t path =
    let open LowLevel in
    with_walk_from_root t path
      (fun newfid ->
        openfid t newfid Types.OpenMode.read_only
        >>*= fun _ ->
        let rec loop acc offset =
          read t newfid offset t.maximum_payload
          >>*= fun { Response.Read.data } ->
          if Cstruct.len data = 0
          then Lwt.return (Ok acc)
          else
            (* Data should be an integral number of marshalled Stat.ts *)
            let module StatArray = Types.Arr(Types.Stat) in
            (Lwt.return (StatArray.read data))
            >>*= fun (stats, rest) ->
            assert (Cstruct.len rest = 0);
            loop (acc @ stats) Int64.(add offset (of_int (Cstruct.len data))) in
        loop [] 0L
    )

  (* 8215 = 8192 + 23 (maximum overhead in a write packet) *)
  let connect flow ?(msize = 8215l) ?(username = "nobody") ?(max_fids=100l) ?(aname = "/") () =
    let reader = Reader.create flow in
    let writer = flow in
    LowLevel.version reader writer msize Types.Version.unix
    >>*= fun version ->
    let msize = min msize version.Response.Version.msize in
    let upgraded = version.Response.Version.version = Types.Version.unix in
    (* Compute the maximum payload size *)
    let smallest_read_response = { Response.tag = Types.Tag.notag; payload = Response.Read { Response.Read.data = Cstruct.create 0 } } in
    let maximum_read_payload = Int32.(sub msize (of_int (Response.sizeof smallest_read_response))) in
    let smallest_write_request = { Request.tag = Types.Tag.notag; payload = Request.Write { Request.Write.data = Cstruct.create 0; fid = Types.Fid.nofid; offset = 0L } } in
    let maximum_write_payload = Int32.(sub msize (of_int (Request.sizeof smallest_write_request))) in
    debug (fun f -> f "Negotiated maximum message size: %ld bytes" msize);
    debug (fun f -> f "Maximum read payload would be: %ld bytes" maximum_read_payload);
    debug (fun f -> f "Maximum write payload would be: %ld bytes" maximum_write_payload);
    (* For compatibility, use the smallest of the two possible maximums *)
    let maximum_payload = min maximum_read_payload maximum_write_payload in
    debug (fun f -> f "We will use a global maximum payload of: %ld bytes" maximum_payload);
    (* We use the convention that fid 0l is the root fid. We'll never clunk
       this one so we can always re-explore the filesystem from the root. *)
    let root = match Types.Fid.of_int32 0l with Ok x -> x | _ -> assert false in
    let shutdown_complete_t, shutdown_complete_wakener = Lwt.task () in
    let t = {
      reader; writer; root; msize; upgraded; maximum_payload;
      transmit_m = Lwt_mutex.create ();
      please_shutdown = false;
      shutdown_m = Lwt_mutex.create ();
      shutdown_complete_t;
      wakeners = Types.Tag.Map.empty;
      free_tags = Types.Tag.recommended;
      free_tags_c = Lwt_condition.create ();
      max_fids; fids = Types.Fid.Set.empty;
      free_fids_c = Lwt_condition.create ();
    } in
    LowLevel.attach reader writer root Types.Fid.nofid username aname None
    >>*= function { Response.Attach.qid } ->
    debug (fun f -> f "Successfully received a root qid: %s" (Sexplib.Sexp.to_string_hum (Types.Qid.sexp_of_t qid)));
    Lwt.async (fun () ->
      let open Lwt.Infix in
      Lwt.catch (fun () ->
        dispatcher_t shutdown_complete_wakener t
        >>= function
        | Error (`Msg m) ->
          err (fun f -> f "dispatcher caught %s: no more responses will be handled" m);
          Lwt.return ()
        | Ok () ->
          Lwt.return ()
      ) (fun e ->
        err (fun f -> f "dispatcher caught %s: no more responses will be handled" (Printexc.to_string e));
        Lwt.return ()
      ) >>= fun () ->
      Lwt.wakeup shutdown_complete_wakener ();
      (* Wake up any existing `rpc` threads blocked on a free fid *)
      Lwt_condition.broadcast t.free_fids_c ();
      (* Notify any remaining blocked threads that we're down *)
      Types.Tag.Map.iter
        (fun tag wakener ->
          info (fun f -> f "Sending disconnection to request with tag %d" (Types.Tag.to_int tag));
          Lwt.wakeup_later wakener (Error (`Msg "connection disconnected"));
          t.wakeners <- Types.Tag.Map.remove tag t.wakeners
          (* Note existing `rpc` threads blocked waiting for a free tag will
             wake up one by one *)
        ) t.wakeners;
      Lwt.return ()
    );
    Lwt.return (Ok t)
end
