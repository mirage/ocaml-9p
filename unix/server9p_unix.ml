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

open Lwt

module Make(Log : S.LOG) = struct
  module Server = Protocol_9p.Server.Make(Log)(Flow_lwt_unix)

  let finally f g =
    Lwt.catch
      (fun () ->
         f () >>= fun result ->
         g () >>= fun _ignored ->
         Lwt.return result
      ) (fun e ->
        g () >>= fun _ignored ->
        Lwt.fail e)

  let accept_forever ?listening ip port f =
    Log.debug "Listening on %s port %d" ip port;
    let s = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
    Lwt_unix.setsockopt s Lwt_unix.SO_REUSEADDR true;
    let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_of_string ip, port) in
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
    finally loop_forever (fun () -> Lwt_unix.close s)

  let serve_forever ?listening ip port receive_cb =
    accept_forever ?listening ip port
      (fun fd ->
         let flow = Flow_lwt_unix.connect fd in
         Server.connect flow ~receive_cb ()
         >>= function
         | Result.Error (`Msg x) -> fail (Failure x)
         | Result.Ok t ->
           Log.debug "Successfully negotiated a connection.";
           Server.after_disconnect t
      )

end
