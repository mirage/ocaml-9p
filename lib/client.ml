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

open Result
open Error
open Infix

module Make(Log: S.LOG)(FLOW: V1_LWT.FLOW) = struct
  module Reader = Buffered9PReader.Make(Log)(FLOW)

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
    shutdown_complete_t: unit Lwt.t;
    mutable wakeners: Response.payload Lwt.u Types.Tag.Map.t;
    mutable free_tags: Types.Tag.Set.t;
    mutable free_fids: Types.Fid.Set.t;
    free_fids_c: unit Lwt_condition.t;
  }

  type connection = t

  (* For converting flow errors *)
  let (>>|=) m f =
    let open Lwt in
    m >>= function
    | `Ok x -> f x
    | `Eof -> return (error_msg "Caught EOF on underlying FLOW")
    | `Error e -> return (error_msg "Unexpected error on underlying FLOW: %s" (FLOW.error_message e))

  let read_one_packet reader =
    Reader.read reader
    >>*= fun buffer ->
    Lwt.return (Response.read buffer)
    >>*= fun (response, _) ->
    let pretty_printed = Sexplib.Sexp.to_string (Response.sexp_of_t response) in
    debug "S %s" pretty_printed;
    Lwt.return (Ok response)

  let write_one_packet flow request =
    debug "C %s" (Request.to_string request);
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

  let rpc t request =
    (* Allocate a fresh tag, or wait if none are available yet *)
    let c = Lwt_condition.create () in
    let rec allocate_tag () =
      let open Lwt in
      if t.free_tags = Types.Tag.Set.empty
      then Lwt_condition.wait c >>= fun () -> allocate_tag ()
      else
        let tag = Types.Tag.Set.min_elt t.free_tags in
        t.free_tags <- Types.Tag.Set.remove tag t.free_tags;
        let th, wakener = Lwt.task () in
        t.wakeners <- Types.Tag.Map.add tag wakener t.wakeners;
        return (tag, th) in
    let deallocate_tag tag =
      t.free_tags <- Types.Tag.Set.add tag t.free_tags;
      t.wakeners <- Types.Tag.Map.remove tag t.wakeners;
      Lwt_condition.signal c ();
      Lwt.return () in
    let with_tag f =
      let open Lwt in
      allocate_tag ()
      >>= fun (tag, th) ->
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
        (* Wait for the response to be read *)
        let open Lwt in
        th >>= fun response ->
        return (Ok response)
      )

  (* The dispatcher thread reads responses from the FLOW and wakes up
     the thread blocked in the rpc function. *)
  let rec dispatcher_t shutdown_complete_wakener t =
    if t.please_shutdown then begin
      Lwt.wakeup shutdown_complete_wakener ();
      Lwt.return (Ok ())
    end else read_one_packet t.reader
    >>*= fun response ->
    let tag = response.Response.tag in
    if not(Types.Tag.Map.mem tag t.wakeners) then begin
      let pretty_printed = Sexplib.Sexp.to_string (Response.sexp_of_t response) in
      error "Received response with unexpected tag: %s" pretty_printed;
      dispatcher_t shutdown_complete_wakener t
    end else begin
      let wakener = Types.Tag.Map.find tag t.wakeners in
      Lwt.wakeup_later wakener response.Response.payload;
      dispatcher_t shutdown_complete_wakener t
    end

  let return_error = function
    | Response.Err { Response.Err.ename } ->
      Lwt.return (Error (`Msg ename))
    | payload ->
      Lwt.return (error_msg "Server sent unexpected reply: %s" (Sexplib.Sexp.to_string (Response.sexp_of_payload  payload)))

  module LowLevel = struct

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

    let version reader writer msize version =
      write_one_packet writer {
        Request.tag = Types.Tag.notag;
        payload = Request.Version Request.Version.({ msize; version });
      } >>*= fun () ->
      read_one_packet reader
      >>*= fun response ->
      match response with
      | { Response.payload = Response.Version v } ->
        Lwt.return (Ok v)
      | { Response.payload = p } -> return_error p

    let attach reader writer fid afid uname aname n_uname =
      let tag = Types.Tag.Set.min_elt Types.Tag.recommended in
      write_one_packet writer {
        Request.tag;
        payload = Request.Attach Request.Attach.({ fid; afid; uname; aname; n_uname })
      } >>*= fun () ->
      read_one_packet reader
      >>*= fun response ->
      match response with
      | { Response.payload = Response.Attach x } ->
        Lwt.return (Ok x)
      | { Response.payload = p } -> return_error p
  end

  let rec allocate_fid t =
    let open Lwt in
    if t.free_fids = Types.Fid.Set.empty
    then Lwt_condition.wait t.free_fids_c >>= fun () -> allocate_fid t
    else
      let fid = Types.Fid.Set.min_elt t.free_fids in
      t.free_fids <- Types.Fid.Set.remove fid t.free_fids;
      return fid
  let deallocate_fid t fid =
    let open Lwt in
    t.free_fids <- Types.Fid.Set.add fid t.free_fids;
    Lwt_condition.signal t.free_fids_c ();
    LowLevel.clunk t fid
    >>= fun _ -> (* the spec says to assume the fid is clunked now *)
    Lwt.return ()
  let with_fid t f =
    let open Lwt in
    allocate_fid t
    >>= fun fid ->
    finally (fun () -> f fid) (fun () -> deallocate_fid t fid)

  let read t path offset count =
    let open LowLevel in
    let fid = t.root in
    with_fid t
      (fun newfid ->
        let wnames = path in
        walk t fid newfid wnames
        >>*= fun _ -> (* I don't need to know the qids *)
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

  let stat t path =
    let open LowLevel in
    let fid = t.root in
    with_fid t
      (fun newfid ->
        let wnames = path in
        walk t fid newfid wnames
        >>*= fun _ -> (* I don't need to know the qids *)
        stat t newfid
        >>*= fun { Response.Stat.stat } ->
        Lwt.return (Ok stat)
      )

  let disconnect t =
    let open Lwt in
    (* Mark the connection as shutting down, so the dispatcher will quit *)
    t.please_shutdown <- true;
    (* Send a request, to unblock the dispatcher *)
    LowLevel.flush t Types.Tag.notag
    >>= fun _ ->
    (* Wait for the dispatcher to shutdown *)
    t.shutdown_complete_t

  module KV_RO = struct
    open Lwt

    type 'a io = 'a Lwt.t

    type t = connection

    type error =
      | Unknown_key of string

    type id = unit

    type page_aligned_buffer = Cstruct.t

    let parse_path x = Stringext.split x ~on:'/'

    let read t key offset length =
      let path = parse_path key in
      let offset = Int64.of_int offset in
      let count = Int32.of_int length in
      read t path offset count
      >>= function
      | Ok bufs -> return (`Ok bufs)
      | _ -> return (`Error (Unknown_key key))

    let size t key =
      let path = parse_path key in
      stat t path
      >>= function
      | Ok stat -> return (`Ok stat.Types.Stat.length)
      | _ -> return (`Error (Unknown_key key))

    let disconnect = disconnect
  end

  let readdir t path =
    let open LowLevel in
    let fid = t.root in
    with_fid t
      (fun newfid ->
        let wnames = path in
        walk t fid newfid wnames
        >>*= fun _ -> (* I don't need to know the qids *)
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
  let connect flow ?(msize = 8215l) ?(username = "nobody") ?(aname = "/") () =
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
    debug "Negotiated maximum message size: %ld bytes" msize;
    debug "Maximum read payload would be: %ld bytes" maximum_read_payload;
    debug "Maximum write payload would be: %ld bytes" maximum_write_payload;
    (* For compatibility, use the smallest of the two possible maximums *)
    let maximum_payload = min maximum_read_payload maximum_write_payload in
    debug "We will use a global maximum payload of: %ld bytes" maximum_payload;
    (* We use the convention that fid 0l is the root fid. We'll never clunk
       this one so we can always re-explore the filesystem from the root. *)
    let root = match Types.Fid.of_int32 0l with Ok x -> x | _ -> assert false in
    let shutdown_complete_t, shutdown_complete_wakener = Lwt.task () in
    let t = {
      reader; writer; root; msize; upgraded; maximum_payload;
      transmit_m = Lwt_mutex.create ();
      please_shutdown = false;
      shutdown_complete_t;
      wakeners = Types.Tag.Map.empty;
      free_tags = Types.Tag.recommended;
      free_fids = Types.Fid.recommended;
      free_fids_c = Lwt_condition.create ();
    } in
    LowLevel.attach reader writer root Types.Fid.nofid username aname None
    >>*= function { Response.Attach.qid } ->
    debug "Successfully received a root qid: %s" (Sexplib.Sexp.to_string_hum (Types.Qid.sexp_of_t qid));
    Lwt.async (fun () -> dispatcher_t shutdown_complete_wakener t);
    Lwt.return (Ok t)
end
