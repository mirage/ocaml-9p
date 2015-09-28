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

let project_url = "http://github.com/djs55/ocaml-9p"
let version = "0.0"

module Client = Client.Make(Flow_lwt_unix)

let with_connection address f =
  let hostname, port =
    try
      let colon = String.index address ':' in
      String.sub address 0 colon, String.sub address (colon + 1) (String.length address - colon - 1)
    with Not_found ->
      address, "5640" in
  Printf.fprintf stderr "Connecting to %s port %s\n%!" hostname port;
  let port = int_of_string port in
  Lwt_unix.gethostbyname hostname
  >>= fun h ->
  ( if Array.length h.Lwt_unix.h_addr_list = 0
    then fail (Failure (Printf.sprintf "gethostbyname returned 0 addresses for '%s'" hostname))
    else return h.Lwt_unix.h_addr_list.(0)
  ) >>= fun inet_addr ->
  let s = Lwt_unix.socket h.Lwt_unix.h_addrtype Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.connect s (Lwt_unix.ADDR_INET(inet_addr, port))
  >>= fun () ->
  Lwt.catch
    (fun () -> f s >>= fun r -> Lwt_unix.close s >>= fun () -> return r)
    (fun e -> Lwt_unix.close s >>= fun () -> fail e)

let ls address path username =
  let t =
    with_connection address
      (fun s ->
        let flow = Flow_lwt_unix.connect s in
        Client.connect flow ?username ()
        >>= function
        | Error (`Msg x) -> failwith x
        | Ok t ->
          Printf.fprintf stderr "Successfully negotiated a connection.\n%!";
          return ()
      ) in
  try
    Lwt_main.run t;
    `Ok ()
  with Failure e ->
    `Error(false, e)
  | e ->
    `Error(false, Printexc.to_string e)

open Cmdliner

let help = [
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]
let address =
  let doc = "Address of the 9P fileserver" in
  Arg.(value & opt string "localhost:5640" & info [ "address"; "a" ] ~doc)

let path =
  let doc = "Path on the 9P fileserver" in
  Arg.(value & pos 0 string "/" & info [] ~doc)

let username =
  let doc = "Username to present to the 9P fileserver" in
  Arg.(value & opt (some string) None & info [ "username"; "u" ] ~doc)

let ls_cmd =
  let doc = "Read a directory" in
  let man = [
    `S "DESCRIPTION";
    `P "List the contents of a directory on the fileserver."
  ] @ help in
  Term.(ret(pure ls $ address $ path $ username)),
  Term.info "ls" ~doc ~man

let default_cmd =
  let doc = "interact with a remote fileserver over 9P" in
  let man = help in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info (Sys.argv.(0)) ~version ~doc ~man

let _ =
  match Term.eval_choice default_cmd [ ls_cmd ] with
  | `Error _ -> exit 1
  | _ -> exit 0
