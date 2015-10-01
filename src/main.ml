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

module Log = struct
  let print_debug = ref false

  let debug fmt = Printf.ksprintf (fun s -> if !print_debug then print_endline s) fmt
  let info  fmt = Printf.ksprintf (fun s -> print_endline s) fmt
  let warn fmt = Printf.ksprintf (fun s -> print_endline s) fmt
  let error fmt = Printf.ksprintf (fun s -> print_endline s) fmt
end

module Client = Client.Make(Log)(Flow_lwt_unix)
module Server = Server.Make(Log)(Flow_lwt_unix)

(* For Result + Lwt *)
let (>>*=) m f =
  let open Lwt in
  m >>= function
  | Result.Ok x -> f x
  | Result.Error x -> Lwt.return (Result.Error x)

let parse_address address =
  try
    let colon = String.index address ':' in
    String.sub address 0 colon, int_of_string (String.sub address (colon + 1) (String.length address - colon - 1))
  with Not_found ->
    address, 5640

let with_connection address f =
  let hostname, port = parse_address address in
  Log.debug "Connecting to %s port %d" hostname port;
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

let finally f g =
  Lwt.catch
    (fun () ->
      f () >>= fun result ->
      g () >>= fun _ignored ->
      Lwt.return result
    ) (fun e ->
      g () >>= fun _ignored ->
      Lwt.fail e)

let accept_forever address f =
  let ip, port = parse_address address in
  Log.debug "Listening on %s port %d" ip port;
  let s = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt s Lwt_unix.SO_REUSEADDR true;
  let sockaddr = Lwt_unix.ADDR_INET(Unix.inet_addr_of_string ip, port) in
  Lwt_unix.bind s sockaddr;
  Lwt_unix.listen s 5;
  let rec loop_forever () =
    Lwt_unix.accept s
    >>= fun (client, _client_addr) ->
    Lwt.async
      (fun () ->
        finally (fun () -> f client) (fun () -> Lwt_unix.close client)
      );
    loop_forever () in
  loop_forever ()

let parse_path x = Stringext.split x ~on:'/'

let with_client address f =
  with_connection address
    (fun s ->
      let flow = Flow_lwt_unix.connect s in
      Client.connect flow ?username ()
      >>= function
      | Error (`Msg x) -> failwith x
      | Ok t ->
        Log.debug "Successfully negotiated a connection.";
        finally (fun () -> f t) (fun () -> Client.disconnect t)
    )

let read debug address path username =
  Log.print_debug := debug;
  let path = parse_path path in
  let t =
    with_client address
      (fun t ->
        let rec loop offset =
          Client.read t path 1024l
          >>*= fun data ->
          if Cstruct.len data = 0
          then return (Ok ())
          else begin
            print_string (Cstruct.to_string data);
            loop Int64.(add offset (of_int (Cstruct.len data)))
          end in
        loop 0L
      ) in
  try
    Lwt_main.run t;
    `Ok ()
  with Failure e ->
    `Error(false, e)
  | e ->
    `Error(false, Printexc.to_string e)


let ls debug address path username =
  Log.print_debug := debug;
  let path = parse_path path in
  let t =
    with_connection address
      (fun s ->
        let flow = Flow_lwt_unix.connect s in
        Client.connect flow ?username ()
        >>= function
        | Error (`Msg x) -> failwith x
        | Ok t ->
          Log.debug "Successfully negotiated a connection.";
          begin Client.readdir t path >>= function
          | Error (`Msg x) -> failwith x
          | Ok stats ->
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
                | 0 -> "Jan" | 1 -> "Feb" | 2 -> "Mar" | 3 -> "Apr" | 4 -> "May" | 5 -> "Jun"
                | 6 -> "Jul" | 7 -> "Aug" | 8 -> "Sep" | 9 -> "Oct" | 10 -> "Nov" | 11 -> "Dec"
                | x -> string_of_int x in
              let day = string_of_int tm.Unix.tm_mday in
              let year = string_of_int (1900 + tm.Unix.tm_year) in
              let name = x.Types.Stat.name in
              Array.of_list [ perms; links; uid; gid; length; month; day; year; name ] in
            let rows = Array.of_list (List.map row_of_stat stats) in
            let padto n x =
              let extra = max 0 (n - (String.length x)) in
              x ^ (String.make extra ' ') in
            Array.iter (fun row ->
              Array.iteri (fun i txt ->
                let column = Array.map (fun row -> row.(i)) rows in
                let biggest = Array.fold_left (fun acc x -> max acc (String.length x)) 0 column in
                Printf.printf "%s " (padto biggest txt)
              ) row;
              Printf.printf "\n";
            ) rows;
            Printf.printf "%!";
            return ()
          end
          >>= fun () ->
          Client.disconnect t
      ) in
  try
    Lwt_main.run t;
    `Ok ()
  with Failure e ->
    `Error(false, e)
  | e ->
    `Error(false, Printexc.to_string e)

let error_callback_cb _ _ =
  Lwt.return (Result.Ok (Response.Err { Response.Err.ename = "whateverrr"; errno = None }))

let serve_local_fs_cb path =
  (* Convert from the fid's path to real paths *)
  let canonicalise root path' =
    let elements = List.filter (fun x -> x <> "" && (x <> ".")) (root @ path') in
    List.fold_left Filename.concat "/" elements in
  (* We need to remember the mapping of Fid to path *)
  let root = canonicalise path [] in
  let fids = ref Types.Fid.Map.empty in
  (* We need to associate files with Qids with unique ids and versions *)
  let qid_of_path realpath =
    Lwt_unix.LargeFile.stat realpath
    >>= fun stats ->
    let open Types.Qid in
    let flags =
        (if stats.Lwt_unix.LargeFile.st_kind = Lwt_unix.S_DIR then [ Directory ] else [])
      @ (if stats.Lwt_unix.LargeFile.st_kind = Lwt_unix.S_LNK then [ Link ] else []) in
    let id = Int64.of_int stats.Lwt_unix.LargeFile.st_ino in
    let version = 0l in
    Lwt.return (Result.Ok { flags; id; version }) in
  let stat_of_path realpath =
    Lwt_unix.LargeFile.stat realpath
    >>= fun stats ->
    qid_of_path realpath
    >>*= fun qid ->
    let mode = Types.FileMode.make
      ~is_directory:(stats.Lwt_unix.LargeFile.st_kind = Lwt_unix.S_DIR)
      ~is_device:(List.mem stats.Lwt_unix.LargeFile.st_kind [ Lwt_unix.S_BLK; Lwt_unix.S_CHR ])
      ~is_symlink:(stats.Lwt_unix.LargeFile.st_kind = Lwt_unix.S_LNK) () in
    let stat = Types.Stat.make ~name:realpath ~qid ~mode () in
    Lwt.return (Result.Ok stat) in

  let callback info request =
    match info, request with
    | info, Request.Walk { Request.Walk.fid; newfid; wnames } ->
      let rec walk dir qids = function
        | [] ->
          fids := Types.Fid.Map.add newfid (canonicalise path wnames) !fids;
          Lwt.return (Result.Ok (Response.Walk { Response.Walk.wqids = List.rev qids }))
        | x :: xs ->
          let realpath = canonicalise path (List.rev (x :: dir)) in
          qid_of_path realpath
          >>*= fun qid ->
          walk (x :: dir) (qid :: qids) xs in
      walk [] [] wnames
    | info, Request.Clunk { Request.Clunk.fid } ->
      fids := Types.Fid.Map.remove fid !fids;
      Lwt.return (Result.Ok (Response.Clunk ()))
    | info, Request.Open { Request.Open.fid; mode } ->
      if not(Types.Fid.Map.mem fid !fids)
      then Lwt.return (Result.Ok (Response.Err { Response.Err.ename = "bad fid"; errno = None }))
      else begin
        qid_of_path (Types.Fid.Map.find fid !fids)
        >>*= fun qid ->
        (* Could do a permissions check here *)
        Lwt.return (Result.Ok (Response.Open { Response.Open.qid; iounit = 512l }))
      end
    | info, Request.Read { Request.Read.fid; offset; count } ->
      if not(Types.Fid.Map.mem fid !fids)
      then Lwt.return (Result.Ok (Response.Err { Response.Err.ename = "bad fid"; errno = None }))
      else begin
        let path = Types.Fid.Map.find fid !fids in
        qid_of_path path
        >>*= fun qid ->
        let buffer = Lwt_bytes.create (Int32.to_int count) in
        if List.mem Types.Qid.Directory qid.Types.Qid.flags then begin
          Lwt_unix.opendir path
          >>= fun h ->
          Lwt_unix.readdir_n h 1024
          >>= fun xs ->
          Lwt_unix.closedir h
          >>= fun () ->
          let rec write off rest = function
            | [] -> Lwt.return (Result.Ok off)
            | x :: xs ->
              stat_of_path (Filename.concat path x)
              >>*= fun stat ->
              let n = Types.Stat.sizeof stat in
              if off < offset
              then write Int64.(add off (of_int n)) rest xs
              else if Cstruct.len rest < n then Lwt.return (Result.Ok off)
              else
                Lwt.return (Types.Stat.write stat rest)
                >>*= fun rest ->
                write Int64.(add off (of_int n)) rest xs in
          let rest = Cstruct.of_bigarray buffer in
          write 0L rest (Array.to_list xs)
          >>*= fun offset' ->
          let data = Cstruct.sub rest 0 Int64.(to_int (sub offset' offset)) in
          Lwt.return (Result.Ok (Response.Read { Response.Read.data }))
        end else begin
          Lwt_unix.openfile path [ Lwt_unix.O_RDONLY ] 0
          >>= fun fd ->
          Lwt_bytes.read fd buffer 0 (Lwt_bytes.length buffer)
          >>= fun n ->
          Lwt_unix.close fd
          >>= fun () ->
          let data = Cstruct.(sub (of_bigarray buffer) 0 n) in
          Lwt.return (Result.Ok (Response.Read { Response.Read.data }))
        end
      end

    | _, _ ->
      Lwt.return (Result.Ok (Response.Err { Response.Err.ename = "not implemented"; errno = None })) in
  (* Translate errors, especially Unix-y ones like ENOENT *)
  fun info request ->
    Lwt.catch
      (fun () -> callback info request)
      (function
       | Unix.Unix_error(err, _, _) ->
         Lwt.return (Result.Ok (Response.Err { Response.Err.ename = Unix.error_message err; errno = None }))
       | e ->
         Lwt.return (Result.Ok (Response.Err { Response.Err.ename = Printexc.to_string e; errno = None })))

let serve debug address path =
  Log.print_debug := debug;
  let path = parse_path path in
  let t =
    accept_forever address
      (fun fd ->
        let flow = Flow_lwt_unix.connect fd in
        Server.connect flow ~receive_cb:(serve_local_fs_cb path) ()
        >>= function
        | Error (`Msg x) -> fail (Failure x)
        | Ok t ->
          Log.debug "Successfully negotiated a connection.";
          let rec loop_forever () =
            Lwt_unix.sleep 60.
            >>= fun () ->
            loop_forever () in
          loop_forever ()
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

let debug =
  let doc = "Enable verbose debugging" in
  Arg.(value & flag & info [ "debug" ] ~doc)

let address =
  let doc = "Address of the 9P fileserver" in
  Arg.(value & opt string "127.0.0.1:5640" & info [ "address"; "a" ] ~doc)

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
  Term.(ret(pure ls $ debug $ address $ path $ username)),
  Term.info "ls" ~doc ~man

let read_cmd =
  let doc = "Read a file" in
  let man = [
    `S "DESCRIPTION";
    `P "Write the contents of a file to stdout.";
  ] @ help in
  Term.(ret(pure read $ debug $ address $ path)),
  Term.info "read" ~doc ~man

let serve_cmd =
  let doc = "Serve a directory over 9P" in
  let man = [
    `S "DESCRIPTION";
    `P "Listen for 9P connections and serve the named filesystem.";
  ] @ help in
  Term.(ret(pure serve $ debug $ address $ path)),
  Term.info "serve" ~doc ~man

let default_cmd =
  let doc = "interact with a remote machine over 9P" in
  let man = help in
  Term.(ret (pure (`Help (`Pager, None)))),
  Term.info (Sys.argv.(0)) ~version ~doc ~man

let _ =
  match Term.eval_choice default_cmd [ ls_cmd; serve_cmd ] with
  | `Error _ -> exit 1
  | _ -> exit 0
