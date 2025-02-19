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
open Protocol_9p_unix
open Infix
open Lwt
open Astring

[@@@warning "-27"]

let project_url = "http://github.com/mirage/ocaml-9p"
let version = "%%VERSION%%"

module Log = (val Logs.src_log Logs.default)
module Client = Client9p_unix.Make(Log)

let () =
  Logs.set_reporter (Logs_fmt.reporter ())

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
  match String.cut ~sep:":" address with
  | Some (proto, address) -> proto, address
  | None -> address, "5640"

let parse_path x = String.cuts x ~sep:"/"

let with_client address username f =
  let proto, address = parse_address address in
  Client.connect proto address ?username ()
  >>= function
  | Error (`Msg x) -> failwith x
  | Ok t ->
    finally (fun () -> f t) (fun () -> Client.disconnect t)

let configure_logging debug =
  if debug then (
    Logs.set_level (Some Logs.Debug);
    Log.debug (fun f -> f "Debug-level logging enabled")
  )


let wrap_exceptions f =
  try
    f ()
  with Failure e ->
    `Error(false, e)
  | Unix.Unix_error(e, _, _) ->
    `Error(false, Win_error.error_message e)
  | e ->
    `Error(false, Printexc.to_string e)

let read debug address path username =
  configure_logging debug;
  let path = parse_path path in
  let mib = Int32.mul 1024l 1024l in
  let two_mib = Int32.mul 2l mib in
  let t =
    with_client address username
      (fun t ->
        let rec loop offset =
          Client.read t path offset two_mib
          >>*= fun data ->
          let len = List.fold_left (+) 0 (List.map (fun x -> Cstruct.length x) data) in
          if len = 0
          then return (Ok ())
          else begin
            List.iter (fun x -> print_string (Cstruct.to_string x)) data;
            loop Int64.(add offset (of_int len))
          end in
        loop 0L
      ) in
  wrap_exceptions
    (fun () ->
      ignore (Lwt_main.run t);
      `Ok ()
    )

let remove debug address path username =
  configure_logging debug;
  let path = parse_path path in
  let t =
    with_client address username
      (fun t ->
        Client.remove t path
        >>*= fun () ->
        return (Ok ())
      ) in
  wrap_exceptions
    (fun () ->
      match Lwt_main.run t with
        | Ok () -> `Ok ()
        | Error (`Msg m) -> `Error(false, m)
    )

let print_stats stats =
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
       | Some { Types.Stat.extension = e; _ } ->
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
    x ^ (String.v ~len:extra (fun _ -> ' ')) in
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
  Printf.printf "%!"

let ls debug address path username =
  configure_logging debug;
  let t =
    with_client address username
      (fun t ->
        Client.readdir t (parse_path path) >>= function
         | Error (`Msg x) -> failwith x
         | Ok stats ->
           print_stats stats;
           return ()
      ) in
  wrap_exceptions
    (fun () ->
      Lwt_main.run t;
      `Ok ()
    )

let cwd = ref []
class read_line ~term ~history ~state = object(self)
  inherit LTerm_read_line.read_line ~history ()
  inherit [Zed_string.t] LTerm_read_line.term term

  method! show_box = false

  initializer
    let open React in
    let open LTerm_text in
    self#set_prompt (S.const (eval [ S (Printf.sprintf "9P %s> " (String.concat ~sep:"/" !cwd)) ]))
end

let shell debug address username =
  configure_logging debug;
  let t =
    with_client address username
      (fun t ->
        let execute_command x =
          match String.cuts ~sep:" " x with
          | [ "ls" ] ->
            begin
              Client.readdir t !cwd >>= function
              | Error (`Msg x) ->
                print_endline x;
                return ()
              | Ok stats ->
                print_stats stats;
                return ()
            end
          | [ "cd"; dir ] ->
            let dir' = String.cuts ~sep:"/" dir in
            let newdir =
              if dir <> "" && dir.[0] = '/' then dir'
              else if dir = "." then !cwd
              else if dir = ".." then (if !cwd = [] then !cwd else List.(rev @@ tl @@ rev !cwd))
              else !cwd @ dir' in
            begin
              Client.stat t newdir
              >>= function
              | Ok x ->
                if x.Protocol_9p.Types.Stat.mode.Protocol_9p.Types.FileMode.is_directory then begin
                  cwd := newdir;
                  return ()
                end else begin
                  Printf.printf "not a directory\n";
                  return ()
                end
              | Error (`Msg m) ->
                print_endline m;
                return ()
            end
          | [ "create"; file ] ->
            let mode = Protocol_9p.Types.FileMode.make ~is_directory:false
              ~owner:[`Read; `Write] ~group:[`Read]
              ~other:[`Read; `Execute ] () in
            begin
              Client.create t !cwd file mode
              >>= function
              | Ok () -> return ()
              | Error (`Msg m) ->
                print_endline m;
                return ()
            end
          | [ "read"; file ]  ->
            let rec copy ofs =
              let requested = 1024l in
              Client.read t (!cwd @ [ file ]) ofs requested
              >>= function
              | Error (`Msg m) ->
                print_endline m;
                return ()
              | Ok bufs ->
                let len = List.fold_left (+) 0 (List.map Cstruct.length bufs) in
                List.iter (fun x -> output_string stdout (Cstruct.to_string x)) bufs;
                flush stdout;
                if Int32.of_int len < requested
                then Lwt.return ()
                else copy Int64.(add ofs (of_int len)) in
            copy 0L
            >>= fun () ->
            return ()
          | "write" :: file :: rest ->
            let data = String.concat ~sep:" " rest in
            let buf = Cstruct.create (String.length data) in
            Cstruct.blit_from_string data 0 buf 0 (Cstruct.length buf);
            begin
              Client.write t (!cwd @ [ file ]) 0L buf
              >>= function
              | Error (`Msg m) ->
                print_endline m;
                return ()
              | Ok () ->
                return ()
            end
          | [ "mkdir"; dir ] ->
            let mode = Protocol_9p.Types.FileMode.make ~is_directory:true
              ~owner:[`Read; `Write; `Execute] ~group:[`Read; `Execute]
              ~other:[`Read; `Execute ] () in
            begin
              Client.mkdir t !cwd dir mode
              >>= function
              | Ok () -> return ()
              | Error (`Msg m) ->
                print_endline m;
                return ()
            end
          | [ "rm"; file ]    ->
              begin
                Client.remove t (!cwd @ [ file ])
                >>= function
                | Ok () -> return ()
                | Error (`Msg m) ->
                  print_endline m;
                  return ()
              end
          | [ "exit" ]  -> exit 0
          | [] -> return ()
          | cmd :: _ -> Printf.printf "Unknown command: %s\n%!" cmd; return () in

        let rec loop term history =
          Lwt.catch
            (fun () ->
              let rl = new read_line ~term ~history:(LTerm_history.contents history) ~state in
              rl#run
              >>= fun command ->
              return (Some command))
            (function
              | Sys.Break -> return None
              | e -> fail e)
          >>= function
          | Some command ->
            execute_command (Zed_string.to_utf8 command)
            >>= fun () ->
            LTerm_history.add history command;
            loop term history
          | None ->
            loop term history in

      LTerm_inputrc.load ()
      >>= fun () ->
      Lwt.catch
        (fun () ->
          Lazy.force LTerm.stdout
          >>= fun term ->
          loop term (LTerm_history.create [])
        ) (function
          | LTerm_read_line.Interrupt -> return ()
          | e -> fail e)
      ) in
  wrap_exceptions
    (fun () ->
      Lwt_main.run t;
      `Ok ()
    )

let serve debug address path =
  configure_logging debug;
  let path = parse_path path in
  let proto, address = parse_address address in
  let t =
    let fs = Lofs9p.make path in
    let module Server = Server9p_unix.Make(Log)(Lofs9p) in
    Server.listen fs proto address
    >>= function
    | Error (`Msg m) -> Lwt.fail (Failure m)
    | Ok server -> Server.serve_forever server in
  wrap_exceptions
    (fun () ->
      ignore (Lwt_main.run t);
      `Ok ()
    )

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
  Term.(ret(const ls $ debug $ address $ path $ username)),
  Cmd.info "ls" ~doc ~man

let read_cmd =
  let doc = "Read a file" in
  let man = [
    `S "DESCRIPTION";
    `P "Write the contents of a file to stdout.";
  ] @ help in
  Term.(ret(const read $ debug $ address $ path $ username)),
  Cmd.info "read" ~doc ~man

let remove_cmd =
  let doc = "Remove a file or directory" in
  let man = [
    `S "DESCRIPTION";
    `P "Remove a file or directory.";
  ] @ help in
  Term.(ret(const remove $ debug $ address $ path $ username)),
  Cmd.info "remove" ~doc ~man

let serve_cmd =
  let doc = "Serve a directory over 9P" in
  let man = [
    `S "DESCRIPTION";
    `P "Listen for 9P connections and serve the named filesystem.";
  ] @ help in
  Term.(ret(const serve $ debug $ address $ path)),
  Cmd.info "serve" ~doc ~man

let shell_cmd =
  let doc = "Run an interactive 9P session" in
  let man = [
    `S "DESCRIPTION";
    `P "Connect to a 9P server and present a shell-like interface."
  ] @ help in
  Term.(ret(const shell $ debug $ address $ username)),
  Cmd.info "shell" ~doc ~man

let default_cmd, default_info =
  let doc = "interact with a remote machine over 9P" in
  let man = help in
  Term.(ret (const (`Help (`Pager, None)))),
  Cmd.info (Sys.argv.(0)) ~version ~doc ~man

let all_cmds = Cmd.group default_info @@ List.map (fun (t, i) -> Cmd.v i t) [
  ls_cmd; read_cmd; remove_cmd; serve_cmd; shell_cmd;
]

let () = 
  exit (Cmd.eval all_cmds)
