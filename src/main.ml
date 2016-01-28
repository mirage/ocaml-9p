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
open Infix
open Lwt

let project_url = "http://github.com/mirage/ocaml-9p"
let version = "0.0"

module Log = Log9p_unix.Stdout
module Client = Client9p_unix.Make(Log)
module Server = Server9p_unix.Make(Log)

let finally f g =
  Lwt.catch
    (fun () ->
      f () >>= fun result ->
      g () >>= fun _ignored ->
      Lwt.return result
    ) (fun e ->
      g () >>= fun _ignored ->
      Lwt.fail e)

let parse_address address =
  match Stringext.split ~on:':' ~max:2 address with
  | [ proto; address ] -> proto, address
  | _ -> address, "5640"

let parse_path x = Stringext.split x ~on:'/'

let with_client address username f =
  let proto, address = parse_address address in
  Client.connect proto address ?username ()
  >>= function
  | Result.Error (`Msg x) -> failwith x
  | Result.Ok t ->
    finally (fun () -> f t) (fun () -> Client.disconnect t)

let read debug address path username =
  Log.print_debug := debug;
  let path = parse_path path in
  let mib = Int32.mul 1024l 1024l in
  let two_mib = Int32.mul 2l mib in
  let t =
    with_client address username
      (fun t ->
        let rec loop offset =
          Client.read t path offset two_mib
          >>*= fun data ->
          let len = List.fold_left (+) 0 (List.map (fun x -> Cstruct.len x) data) in
          if len = 0
          then return (Result.Ok ())
          else begin
            List.iter (fun x -> print_string (Cstruct.to_string x)) data;
            loop Int64.(add offset (of_int len))
          end in
        loop 0L
      ) in
  try
    ignore (Lwt_main.run t);
    `Ok ()
  with Failure e ->
    `Error(false, e)
  | e ->
    `Error(false, Printexc.to_string e)

let remove debug address path username =
  Log.print_debug := debug;
  let path = parse_path path in
  let t =
    with_client address username
      (fun t ->
        Client.remove t path
        >>*= fun () ->
        return (Result.Ok ())
      ) in
  try
    Result.(match Lwt_main.run t with
      | Ok () -> `Ok ()
      | Error (`Msg m) -> `Error(false, m)
    )
  with Failure e ->
    `Error(false, e)
  | e ->
    `Error(false, Printexc.to_string e)

let do_ls t path =
  let path = parse_path path in
  Client.readdir t path >>= function
   | Result.Error (`Msg x) -> failwith x
   | Result.Ok stats ->
     let row_of_stat x =
       let permissions p =
         (if List.mem `Read p then "r" else "-")
         ^ (if List.mem `Write p then "w" else "-")
         ^ (if List.mem `Execute p then "x" else "-") in
       let filemode = x.Types.Stat.mode in
       let owner = permissions filemode.Types.FileMode.owner in
       let group = permissions filemode.Types.FileMode.group in
       let other = permissions filemode.Types.FileMode.other in
       let kind =
         let open Types.FileMode in
         if filemode.is_directory then "d"
         else if filemode.is_symlink then "l"
         else if filemode.is_device then "c"
         else if filemode.is_socket then "s"
         else "-" in
       let perms = kind ^ owner ^ group ^ other in
       let links = "?" in
       let uid = x.Types.Stat.uid in
       let gid = x.Types.Stat.gid in
       let length = Int64.to_string x.Types.Stat.length in
       let tm = Unix.gmtime (Int32.to_float x.Types.Stat.mtime) in
       let month = match tm.Unix.tm_mon with
         | 0 -> "Jan"  | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr" | 4 -> "May"
         | 5 -> "Jun"  | 6 -> "Jul" | 7 -> "Aug" | 8 -> "Sep" | 9 -> "Oct"
         | 10 -> "Nov" | 11 -> "Dec"
         | x -> string_of_int x
       in
       let day = string_of_int tm.Unix.tm_mday in
       let year = string_of_int (1900 + tm.Unix.tm_year) in
       let name =
        let name = x.Types.Stat.name in
        if filemode.Types.FileMode.is_symlink
        then match x.Types.Stat.u with
          | Some { Types.Stat.extension = e } ->
            name ^ " -> " ^ e
          | None ->
            name
        else name in
       Array.of_list [
         perms; links; uid; gid; length; month; day; year; name;
       ] in
     let rows = Array.of_list (List.map row_of_stat stats) in
     let padto n x =
       let extra = max 0 (n - (String.length x)) in
       x ^ (String.make extra ' ') in
     Array.iter (fun row ->
       Array.iteri (fun i txt ->
         let column = Array.map (fun row -> row.(i)) rows in
         let biggest = Array.fold_left (fun acc x ->
           max acc (String.length x)
         ) 0 column in
         Printf.printf "%s " (padto biggest txt)
       ) row;
       Printf.printf "\n";
     ) rows;
     Printf.printf "%!";
     return ()

let ls debug address path username =
  Log.print_debug := debug;
  let t =
    with_client address username
      (fun t -> do_ls t path) in
  try
    Lwt_main.run t;
    `Ok ()
  with Failure e ->
    `Error(false, e)
  | e ->
    `Error(false, Printexc.to_string e)

let shell debug address username =
  Log.print_debug := debug;
  let t =
    with_client address username
      (fun t ->
        let cwd = ref "/" in
        let unimplemented fn =
          Printf.printf "%s is not implemented.\n" fn;
          Lwt.return () in

        let rec loop () =
          Printf.printf "9P:%s> %!" !cwd;
          match Stringext.split ~on:' ' (input_line stdin) with
          | [ "ls" ]      -> do_ls t !cwd           >>= fun () -> loop ()
          | [ "cd"; dir ] ->
            let newdir = Filename.concat !cwd dir in
            begin
              Client.stat t (parse_path newdir)
              >>= function
              | Result.Ok x ->
                if x.Protocol_9p_types.Stat.mode.Protocol_9p_types.FileMode.is_directory then begin
                  cwd := newdir;
                  loop ()
                end else begin
                  Printf.printf "not a directory\n";
                  loop ()
                end
              | Result.Error _ ->
                Printf.printf "does not exist\n";
                loop ()
            end
          | [ "read" ]  -> unimplemented "read"   >>= fun () -> loop ()
          | [ "write" ] -> unimplemented "write"  >>= fun () -> loop ()
          | [ "mkdir" ] -> unimplemented "mkdir"  >>= fun () -> loop ()
          | [ "rmdir" ] -> unimplemented "rmdir"  >>= fun () -> loop ()
          | [ "rm" ]    -> unimplemented "rm"     >>= fun () -> loop ()
          | [ "exit" ]  -> return () (* terminate loop *)
          | [] -> loop ()
          | cmd :: _ -> Printf.printf "Unknown command: %s\n%!" cmd; loop () in
        loop ()
      ) in
  try
    Lwt_main.run t;
    `Ok ()
  with Failure e ->
    `Error(false, e)
  | e ->
    `Error(false, Printexc.to_string e)

let serve_local_fs_cb path =
  let module Lofs = Lofs9p.New(struct let root = path end) in
  let module Fs = Handler.Make(Lofs) in
  (* Translate errors, especially Unix-y ones like ENOENT *)
  fun info ~cancel request ->
    Lwt.catch
      (fun () -> Fs.receive_cb info ~cancel request)
      (fun exn ->
         let is_unix = (info.Protocol_9p.Server.version = Types.Version.unix) in
         match exn with
         | Unix.Unix_error(err, _, _) ->
           let host = match info.Protocol_9p.Server.aname with
             | "linux#/" when is_unix -> Some Errno_host.Linux.v4_0_5
             | "osx#/" when is_unix -> Some Errno_host.OSX.v10_11_1
             | _ -> None
           in
           let errno = match host with
             | None -> None
             | Some host -> match Errno_unix.of_unix ~host err with
               | [] -> None
               | errno::_ -> match Errno.to_code ~host errno with
                 | None -> None
                 | Some i -> Some (Int32.of_int i)
           in
           Lwt.return (Result.Ok (Response.Err {
             Response.Err.ename = Unix.error_message err;
             errno;
           }))
         | e ->
           Lwt.return (Result.Ok (Response.Err {
             Response.Err.ename = Printexc.to_string e;
             errno = None;
           })))

let serve debug address path =
  Log.print_debug := debug;
  let path = parse_path path in
  let proto, address = parse_address address in
  let t =
    Server.listen proto address (serve_local_fs_cb path)
    >>= function
    | Result.Error (`Msg m) -> Lwt.fail (Failure m)
    | Result.Ok server -> Server.serve_forever server in
  try
    ignore (Lwt_main.run t);
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

let debug =
  let doc = "Enable verbose debugging" in
  Arg.(value & flag & info [ "debug" ] ~doc)

let address =
  let doc = "Address of the 9P fileserver" in
  Arg.(value & opt string "tcp:127.0.0.1:5640" & info [ "address"; "a" ] ~doc)

let path =
  let doc = "Path on the 9P fileserver" in
  Arg.(value & pos 0 string "" & info [] ~doc)

let username =
  let doc = "Username to present to the 9P fileserver" in
  Arg.(value & opt (some string) None & info [ "username"; "u" ] ~doc)

let ls_cmd =
  let doc = "Read a directory" in
  let man = [
    `S "DESCRIPTION";
    `P "List the contents of a directory on the fileserver."
  ] @ help in
  Term.(ret(pure ls $ debug $ address $ path $ username)),
  Term.info "ls" ~doc ~man

let read_cmd =
  let doc = "Read a file" in
  let man = [
    `S "DESCRIPTION";
    `P "Write the contents of a file to stdout.";
  ] @ help in
  Term.(ret(pure read $ debug $ address $ path $ username)),
  Term.info "read" ~doc ~man

let remove_cmd =
  let doc = "Remove a file or directory" in
  let man = [
    `S "DESCRIPTION";
    `P "Remove a file or directory.";
  ] @ help in
  Term.(ret(pure remove $ debug $ address $ path $ username)),
  Term.info "remove" ~doc ~man

let serve_cmd =
  let doc = "Serve a directory over 9P" in
  let man = [
    `S "DESCRIPTION";
    `P "Listen for 9P connections and serve the named filesystem.";
  ] @ help in
  Term.(ret(pure serve $ debug $ address $ path)),
  Term.info "serve" ~doc ~man

let shell_cmd =
  let doc = "Run an interactive 9P session" in
  let man = [
    `S "DESCRIPTION";
    `P "Connect to a 9P server and present a shell-like interface."
  ] @ help in
  Term.(ret(pure shell $ debug $ address $ username)),
  Term.info "shell" ~doc ~man

let default_cmd =
  let doc = "interact with a remote machine over 9P" in
  let man = help in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info (Sys.argv.(0)) ~version ~doc ~man

let all_cmds = [
  ls_cmd; read_cmd; remove_cmd; serve_cmd; shell_cmd;
]

let _ =
  match Term.eval_choice default_cmd all_cmds with
  | `Error _ -> exit 1
  | _ -> exit 0
