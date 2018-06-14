(*
 * Copyright (C) 2015 David Sheets <david.sheets@unikernel.com>
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
open Lwt
open Protocol_9p
open Astring
open Protocol_9p.Infix

module Metrics = struct
  let namespace = "ocaml9p"
  let subsystem = "client"

  let server_ping_time_seconds =
    let help = "Time to receive a response to a ping message" in
    Prometheus.Summary.v ~help ~namespace ~subsystem "ping_time_seconds"
end

module Make(Log: S.LOG) = struct

  module Client = Client.Make(Log)(Flow_lwt_unix)

  type t = {
    client: Client.t;
    flow: Flow_lwt_unix.flow;
    switch : Lwt_switch.t;
  }

  type connection = t

  let pp_addr ppf = function
    | Lwt_unix.ADDR_UNIX path -> Fmt.pf ppf "unix:%s" path
    | Lwt_unix.ADDR_INET (host, port) ->
      Fmt.pf ppf "tcp:%s:%d" (Unix.string_of_inet_addr host) port

  let connect_or_close s addr =
    Lwt.catch
      (fun () -> Lwt_unix.connect s addr >|= fun () -> Ok s)
      (fun ex ->
         Lwt_unix.close s >|= fun () ->
         Protocol_9p.Error.error_msg "Error connecting socket to 9p endpoint %a: %s"
           pp_addr addr (Printexc.to_string ex)
      )

  let open_tcp hostname port =
    Lwt_unix.gethostbyname hostname
    >>= fun h ->
    (* This should probably be a Result error and not an Lwt error. *)
    ( if Array.length h.Lwt_unix.h_addr_list = 0
      then
        let msg =
          Printf.sprintf "gethostbyname returned 0 addresses for '%s'" hostname
        in
        fail (Failure msg)
      else return h.Lwt_unix.h_addr_list.(0)
    ) >>= fun inet_addr ->
    let s = Lwt_unix.socket h.Lwt_unix.h_addrtype Lwt_unix.SOCK_STREAM 0 in
    connect_or_close s (Lwt_unix.ADDR_INET (inet_addr, port))

  let open_unix path =
    let s = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    connect_or_close s (Lwt_unix.ADDR_UNIX path)

  let rec ping_thread ~switch t =
    Client.with_fid t.client (fun newfid ->
        let t0 = Unix.gettimeofday () in
        Client.walk_from_root t.client newfid [] >>*= fun _ ->
        Prometheus.Summary.observe Metrics.server_ping_time_seconds (Unix.gettimeofday () -. t0);
        Lwt.return (Ok ())
      )
    >>= fun result ->
    if Lwt_switch.is_on switch then (
      match result with
      | Ok () ->
        Lwt_unix.sleep 30.0 >>= fun () ->
        ping_thread ~switch t
      | Error (`Msg e) ->
        Log.err (fun f -> f "Ping failed: %s" e);
        Lwt_switch.turn_off switch
    ) else (
      Lwt.return_unit
    )

  let connect proto address ?msize ?username ?aname ?max_fids ?(send_pings=false) () =
    ( match proto, address with
      | "tcp", _ ->
        begin match String.cuts ~sep:":" address with
          | [ hostname; port ] -> open_tcp hostname (int_of_string port)
          | [ hostname ]       -> open_tcp hostname 5640
          | _ ->
            Lwt.fail_with (Printf.sprintf "Unable to parse %s %s" proto address)
        end
      | "unix", _ ->
        open_unix address
      | _ ->
        Lwt.return (Error.error_msg "Unknown protocol %s" proto)
    ) >>*= fun s ->
    let flow = Flow_lwt_unix.connect s in
    Client.connect flow ?msize ?username ?aname ?max_fids ()
    >>= function
    | Result.Error _ as err -> Lwt.return err
    | Result.Ok client ->
      Log.debug (fun f -> f "Successfully negotiated a connection.");
      let switch = Lwt_switch.create () in
      let t = { client; flow; switch } in
      Lwt_switch.add_hook (Some switch)
        (fun () ->
           Client.disconnect client
           >>= fun () ->
           Flow_lwt_unix.close flow
        );
      if send_pings then Lwt.async (fun () -> ping_thread ~switch t);
      Lwt.return (Result.Ok t)

  let after_disconnect { client } = Client.after_disconnect client

  let disconnect t =
    Lwt_switch.turn_off t.switch

  let create { client } = Client.create client

  let read { client } = Client.read client

  let write { client } = Client.write client

  let mkdir { client } = Client.mkdir client

  let remove { client } = Client.remove client

  let readdir { client } = Client.readdir client

  let stat { client } = Client.stat client

  module KV_RO = struct
    open Client

    type t = connection

    type 'a io = 'a KV_RO.io

    type error = KV_RO.error

    let pp_error = KV_RO.pp_error

    type page_aligned_buffer = KV_RO.page_aligned_buffer

    let disconnect { client } = KV_RO.disconnect client

    let read { client } = KV_RO.read client

    let size { client } = KV_RO.size client

    let mem { client } = KV_RO.mem client
  end

  module LowLevel = struct
    open Client

    let maximum_write_payload { client } = LowLevel.maximum_write_payload client

    let allocate_fid { client } = LowLevel.allocate_fid client

    let deallocate_fid { client } = LowLevel.deallocate_fid client

    let walk { client } = LowLevel.walk client

    let openfid { client } = LowLevel.openfid client

    let create { client } = LowLevel.create client

    let stat { client } = LowLevel.stat client

    let wstat { client } = LowLevel.wstat client

    let read { client } = LowLevel.read client

    let write { client } = LowLevel.write client

    let clunk { client } = LowLevel.clunk client

    let remove { client } = LowLevel.remove client

    let update { client } = LowLevel.update client
  end

  let walk_from_root { client } = Client.walk_from_root client
  let with_fid { client } = Client.with_fid client
end
