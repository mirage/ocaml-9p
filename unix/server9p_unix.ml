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

module Make(Log : S.LOG) = struct
  module S = Protocol_9p.Server.Make(Log)(Flow_lwt_unix)

  type ip = string
  type port = int

  let finally f g =
    Lwt.catch
      (fun () ->
         f () >>= fun result ->
         g () >>= fun _ignored ->
         Lwt.return result
      ) (fun e ->
        g () >>= fun _ignored ->
        Lwt.fail e)

  type t = {
    shutdown_requested_t: unit Lwt.t;
    shutdown_requested_u: unit Lwt.u;
    shutdown_done_t: unit Lwt.t;
    shutdown_done_u: unit Lwt.u;
    ip: string;
    port: int;
    receive_cb: Server.receive_cb;
  }

  let create ip port receive_cb =
    let shutdown_requested_t, shutdown_requested_u = Lwt.task () in
    let shutdown_done_t, shutdown_done_u = Lwt.task () in
    { shutdown_requested_t; shutdown_requested_u;
      shutdown_done_t; shutdown_done_u;
      ip; port; receive_cb }

  let shutdown t =
    Lwt.wakeup_later t.shutdown_requested_u ();
    t.shutdown_done_t

  let accept_forever ?listening t f =
    Log.debug "Listening on %s port %d" t.ip t.port;
    let s = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt s Lwt_unix.SO_REUSEADDR true;
    let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_of_string t.ip, t.port) in
    Lwt_unix.bind s sockaddr;
    Lwt_unix.listen s 5;
    (match listening with
     | Some listening -> wakeup listening ()
     | None -> ()
    );
    let rec loop_forever () =
      Lwt_unix.accept s
      >>= fun (client, _client_addr) ->
      print_endline "accepted connection";
      Lwt.async
        (fun () ->
           finally (fun () -> f client) (fun () -> Lwt_unix.close client)
        );
      loop_forever ()
    in
    finally
      (fun () ->
        Lwt.pick [ loop_forever (); t.shutdown_requested_t ]
      ) (fun () ->
        Lwt_unix.close s
      )
    >>= fun () ->
    Lwt.wakeup_later t.shutdown_done_u ();
    return ()

  let serve_forever ?listening t =
    accept_forever ?listening t
      (fun fd ->
         let flow = Flow_lwt_unix.connect fd in
         S.connect flow ~receive_cb:t.receive_cb ()
         >>= function
         | Result.Error (`Msg x) -> fail (Failure x)
         | Result.Ok t ->
           Log.debug "Successfully negotiated a connection.";
           S.after_disconnect t
      )

end
