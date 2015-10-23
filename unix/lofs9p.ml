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

(* We need to associate files with Qids with unique ids and versions *)
let qid_of_path realpath =
  Lwt_unix.LargeFile.stat realpath
  >>= fun stats ->
  let open Types.Qid in
  let flags =
    (if stats.Lwt_unix.LargeFile.st_kind = Lwt_unix.S_DIR
     then [ Directory ]
     else []) @
    (if stats.Lwt_unix.LargeFile.st_kind = Lwt_unix.S_LNK
     then [ Link ]
     else []) in
  let id = Int64.of_int stats.Lwt_unix.LargeFile.st_ino in
  let version = 0l in
  Lwt.return (Result.Ok { flags; id; version })

let bad_fid = Lwt.return (Result.Ok (Response.Err {
  Response.Err.ename = "bad fid";
  errno = None;
}))

module New(Params : sig val root : string list end) = struct
  module Path = struct
    type t = string list

    (* Convert from a segmented path to real paths *)
    let realpath path =
      let root = Params.root in
      let elements = List.filter (fun x -> x <> "" && x <> ".") (root @ path) in
      List.fold_left Filename.concat "/" elements

    let stat name realpath =
      let open Lwt_unix in
      LargeFile.stat realpath
      >>= fun stats ->
      qid_of_path realpath
      >>*= fun qid ->
      let is_device = List.mem stats.LargeFile.st_kind [ S_BLK; S_CHR ] in
      let mode = Types.FileMode.make
          ~is_directory:(stats.LargeFile.st_kind = S_DIR)
          ~is_device
          ~is_symlink:(stats.LargeFile.st_kind = S_LNK) () in
      let name = if name = "" then "/" else name in
      let stat = Types.Stat.make ~name ~qid ~mode () in
      Lwt.return (Result.Ok stat)

  end

  (* We need to remember the mapping of Fid to path *)
  let fids : Path.t Types.Fid.Map.t ref = ref Types.Fid.Map.empty

  let path_of_fid info fid =
    if fid = info.Server.root
    then []
    else Types.Fid.Map.find fid !fids

  let read info { Request.Read.fid; offset; count } =
    match path_of_fid info fid with
    | exception Not_found -> bad_fid
    | path ->
      let realpath = Path.realpath path in
      qid_of_path realpath
      >>*= fun qid ->
      let buffer = Lwt_bytes.create (Int32.to_int count) in
      if List.mem Types.Qid.Directory qid.Types.Qid.flags then begin
        Lwt_unix.opendir realpath
        >>= fun h ->
        Lwt_unix.readdir_n h 1024
        >>= fun xs ->
        Lwt_unix.closedir h
        >>= fun () ->
        let rec write off rest = function
          | [] -> Lwt.return (Result.Ok off)
          | x :: xs ->
            Path.stat x (Path.realpath path)
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
        Lwt_unix.openfile realpath [ Lwt_unix.O_RDONLY ] 0
        >>= fun fd ->
        Lwt_unix.LargeFile.lseek fd offset Lwt_unix.SEEK_SET
        >>= fun _ ->
        Lwt_bytes.read fd buffer 0 (Lwt_bytes.length buffer)
        >>= fun n ->
        Lwt_unix.close fd
        >>= fun () ->
        let data = Cstruct.(sub (of_bigarray buffer) 0 n) in
        Lwt.return (Result.Ok (Response.Read { Response.Read.data }))
      end

  let open_ info { Request.Open.fid; mode } =
    match path_of_fid info fid with
    | exception Not_found -> bad_fid
    | path ->
      let realpath = Path.realpath path in
      qid_of_path realpath
      >>*= fun qid ->
      (* Could do a permissions check here *)
      Lwt.return (Result.Ok (Response.Open { Response.Open.qid; iounit = 512l }))

  let clunk info { Request.Clunk.fid } =
    fids := Types.Fid.Map.remove fid !fids;
    Lwt.return (Result.Ok (Response.Clunk ()))

  let walk info { Request.Walk.fid; newfid; wnames } =
    let rec walk dir qids = function
      | [] ->
        fids := Types.Fid.Map.add newfid wnames !fids;
        Lwt.return (Result.Ok (Response.Walk { Response.Walk.wqids = List.rev qids }))
      | x :: xs ->
        let realpath = Path.realpath (List.rev (x :: dir)) in
        qid_of_path realpath
        >>*= fun qid ->
        walk (x :: dir) (qid :: qids) xs in
    walk [] [] wnames

  let stat info { Request.Stat.fid } =
    match path_of_fid info fid with
    | exception Not_found -> bad_fid
    | [] ->
      Path.stat "" (Path.realpath [])
      >>*= fun stat ->
      Lwt.return (Result.Ok Response.(Stat { Stat.stat }))
    | path ->
      let rpath = List.rev path in
      Path.stat (List.hd rpath) (Path.realpath List.(rev (tl rpath)))
      >>*= fun stat ->
      Lwt.return (Result.Ok Response.(Stat { Stat.stat }))

end
