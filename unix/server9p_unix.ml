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

module Make(Log : S.LOG)(Filesystem: Filesystem.S) = struct
  module S = Protocol_9p.Server.Make(Log)(Flow_lwt_unix)(Filesystem)

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
    mutable fd: Lwt_unix.file_descr option;
    fs: Filesystem.t;
  }

  let of_fd fs fd =
    let shutdown_requested_t, shutdown_requested_u = Lwt.task () in
    let shutdown_done_t, shutdown_done_u = Lwt.task () in
    let fd = Some fd in
    { shutdown_requested_t; shutdown_requested_u;
      shutdown_done_t; shutdown_done_u;
      fd; fs }

  let listen fs proto address = match proto with
    | "tcp" ->
      begin match Stringext.split ~on:':' address with
      | [ ip; port ] ->
        let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
        Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
        let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_of_string ip, int_of_string port) in
        Lwt_unix.bind fd sockaddr;
        Lwt_unix.listen fd 5;
        Lwt.return (Result.Ok (of_fd fs fd))
      | _ ->
        Lwt.return (Error.error_msg "Unable to understand protocol %s and address %s" proto address)
      end
    | "unix" ->
      Lwt.catch (fun () -> Lwt_unix.unlink address) (fun _ -> Lwt.return ())
      >>= fun () ->
      let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
      let sockaddr = Lwt_unix.ADDR_UNIX(address) in
      Lwt_unix.bind fd sockaddr;
      Lwt_unix.listen fd 5;
      Lwt.return (Result.Ok (of_fd fs fd))
    | _ ->
      Lwt.return (Error.error_msg "Unknown protocol %s" proto)

  let shutdown t =
    Lwt.wakeup_later t.shutdown_requested_u ();
    t.shutdown_done_t

  let accept_forever t f =
    match t.fd with
    | None ->
      Lwt.return (Error.error_msg "9P server already shutdown")
    | Some fd ->
      let rec loop_forever () =
        Lwt_unix.accept fd
        >>= fun (client, _client_addr) ->
        Log.debug (fun f -> f "accepted connection");
        Lwt.async (fun () ->
          Lwt.catch (fun () ->
             finally (fun () -> f client) (fun () -> Lwt_unix.close client)
          ) (fun e ->
            Log.err (fun f -> f "server loop caught %s: no further requests will be processed" (Printexc.to_string e));
            Lwt.return ()
          )
        );
        loop_forever ()
      in
      finally
        (fun () ->
          Lwt.pick [ loop_forever (); t.shutdown_requested_t ]
        ) (fun () ->
          t.fd <- None;
          Lwt_unix.close fd
        )
      >>= fun () ->
      Lwt.wakeup_later t.shutdown_done_u ();
      return (Result.Ok ())

  let serve_forever t =
    accept_forever t
      (fun fd ->
         let flow = Flow_lwt_unix.connect fd in
         S.connect t.fs flow ()
         >>= function
         | Result.Error (`Msg x) -> fail (Failure x)
         | Result.Ok t ->
           Log.debug (fun f -> f "Successfully negotiated a connection.");
           S.after_disconnect t
      )

end
