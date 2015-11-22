(*
 * Copyright (c) 2015 David Sheets <sheets@alum.mit.edu>
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
open Infix
open Result
open OUnit

module LogServer  = Log9p_unix.StdoutPrefix(struct let prefix = "S" end)
module LogClient1 = Log9p_unix.StdoutPrefix(struct let prefix = "C1" end)
module LogClient2 = Log9p_unix.StdoutPrefix(struct let prefix = "C2" end)
module Server = Server9p_unix.Make(LogServer)
module Client1 = Client9p_unix.Inet(LogClient1)
module Client2 = Client9p_unix.Inet(LogClient2)

let ip = "127.0.0.1"
let port = 5640

let serve_local_fs_cb path =
  let module Lofs = Lofs9p.New(struct let root = path end) in
  let module Fs = Handler.Make(Lofs) in
  (* Translate errors, especially Unix-y ones like ENOENT *)
  fun info request ->
    Lwt.catch
      (fun () -> Fs.receive_cb info request)
      (function
       | Unix.Unix_error(err, _, _) ->
         Lwt.return (Result.Ok (Response.Err {
           Response.Err.ename = Unix.error_message err;
           errno = None;
         }))
       | e ->
         Lwt.return (Result.Ok (Response.Err {
           Response.Err.ename = Printexc.to_string e;
           errno = None;
         })))

let server_down = ref false
let start_server listening =
  server_down := false;
  let path = ["tmp"] in
  Server.serve_forever ~listening ip port (serve_local_fs_cb path)

let server_tear_down pid =
  if not !server_down
  then begin
    Unix.kill pid Sys.sigterm;
    let rec wait () =
      let result =
        try
          Some (Unix.waitpid [] pid)
        with
        | Unix.Unix_error(Unix.EINTR, _, _) ->
          None in
      match result with
      | Some x -> x
      | None -> wait () in
    let _pid, _status = wait () in
    (* Don't kill child pids in the signal handlers, since the child is dead *)
    Sys.(set_signal sigterm (Signal_handle (fun _sig ->
      exit 0
    )));
    Sys.(set_signal sigint (Signal_handle (fun _sig ->
      exit 0
    )));
    Unix.rmdir "tmp";
    server_down := true
  end

let server_setup () =
  (try Unix.mkdir "tmp" 0o700 with Unix.Unix_error(Unix.EEXIST, _, _) -> ());
  let from_child, to_parent = Unix.pipe () in
  let ready_msg = Bytes.of_string "ready" in
  match Lwt_unix.fork () with
  | 0 ->
    Unix.close from_child;
    begin
      try
        Lwt_main.run (
          let ready, wakener = wait () in
          async (fun () ->
            ready
            >>= fun () ->
            let to_parent = Lwt_unix.of_unix_file_descr to_parent in
            Lwt_unix.write to_parent ready_msg 0 5
            >>= fun _written ->
            return_unit
          );
          start_server wakener
          >>= fun () ->
          exit 0
        )
      with e ->
        (* We really don't want an exception to propagate by accident *)
        Printf.fprintf stderr "child failed with %s\n%!" (Printexc.to_string e);
        exit 0
    end
  | child ->
    Unix.close to_parent;
    Sys.(set_signal sigterm (Signal_handle (fun _sig ->
      server_tear_down child;
      exit 0
    )));
    Sys.(set_signal sigint (Signal_handle (fun _sig ->
      server_tear_down child;
      exit 0
    )));
    let buf = Bytes.create 5 in
    let _read = Unix.read from_child buf 0 5 in
    if buf = ready_msg
    then child
    else
      let msg = Bytes.to_string buf in
      assert_failure ("couldn't confirm server startup: "^msg)

let with_server test =
  bracket server_setup (fun _ -> Lwt_main.run (test ()))
    (fun child ->
      server_tear_down child;
      server_down := false
    )

let with_client1 f =
  Client1.connect ip port ()
  >>= function
  | Error (`Msg err) -> assert_failure ("client1: "^err)
  | Ok client ->
    Lwt.catch (fun () ->
      f client
      >>= fun () ->
      Client1.disconnect client
    ) (fun exn ->
      Client1.disconnect client
      >>= fun () ->
      fail exn
    )

let with_client2 f =
  Client2.connect ip port ()
  >>= function
  | Error (`Msg err) -> assert_failure ("client2: "^err)
  | Ok client ->
    Lwt.catch (fun () ->
      f client
      >>= fun () ->
      Client2.disconnect client
    ) (fun exn ->
      Client2.disconnect client
      >>= fun () ->
      fail exn
    )

let connect1 () =
  with_client1 (fun _client1 -> return_unit)

let connect2 () =
  with_client1 (fun _client1 ->
    with_client2 (fun _client2 -> return_unit)
  )

let () = LogServer.print_debug := false
let () = LogClient1.print_debug := false
let () = LogClient2.print_debug := false

let tests =
  let connect1 = "connect1" >:: (with_server connect1) in
  let connect2 = "connect2" >:: (with_server connect2) in
  [ connect1; connect2 ]

let () =
  let suite = "client server" >::: tests in
  
  OUnit2.run_test_tt_main (ounit2_of_ounit1 suite)
