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

(* Convert from the fid's path to real paths *)
let canonicalise root path' =
  let elements = List.filter (fun x -> x <> "" && (x <> ".")) (root @ path') in
  List.fold_left Filename.concat "/" elements

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

let stat_of_path realpath =
  Lwt_unix.LargeFile.stat realpath
  >>= fun stats ->
  qid_of_path realpath
  >>*= fun qid ->
  let is_device =
    List.mem stats.Lwt_unix.LargeFile.st_kind [ Lwt_unix.S_BLK; Lwt_unix.S_CHR ]
  in
  let mode = Types.FileMode.make
      ~is_directory:(stats.Lwt_unix.LargeFile.st_kind = Lwt_unix.S_DIR)
      ~is_device
      ~is_symlink:(stats.Lwt_unix.LargeFile.st_kind = Lwt_unix.S_LNK) () in
  let stat = Types.Stat.make ~name:realpath ~qid ~mode () in
  Lwt.return (Result.Ok stat)

let bad_fid = Lwt.return (Result.Ok (Response.Err {
  Response.Err.ename = "bad fid";
  errno = None;
}))

module New(Params : sig val root : string list end) = struct
  (* We need to remember the mapping of Fid to path *)
  (* let root = canonicalise path [] in*)
  let fids = ref Types.Fid.Map.empty

  let read info { Request.Read.fid; offset; count } =
    if not(Types.Fid.Map.mem fid !fids)
    then bad_fid
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
        Lwt_unix.LargeFile.lseek fd offset Lwt_unix.SEEK_SET
        >>= fun _ ->
        Lwt_bytes.read fd buffer 0 (Lwt_bytes.length buffer)
        >>= fun n ->
        Lwt_unix.close fd
        >>= fun () ->
        let data = Cstruct.(sub (of_bigarray buffer) 0 n) in
        Lwt.return (Result.Ok (Response.Read { Response.Read.data }))
      end
    end

  let open_ info { Request.Open.fid; mode } =
    if not(Types.Fid.Map.mem fid !fids)
    then bad_fid
    else begin
      qid_of_path (Types.Fid.Map.find fid !fids)
      >>*= fun qid ->
      (* Could do a permissions check here *)
      Lwt.return (Result.Ok (Response.Open { Response.Open.qid; iounit = 512l }))
    end

  let clunk info { Request.Clunk.fid } =
    fids := Types.Fid.Map.remove fid !fids;
    Lwt.return (Result.Ok (Response.Clunk ()))

  let walk info { Request.Walk.fid; newfid; wnames } =
    let rec walk dir qids = function
      | [] ->
        fids := Types.Fid.Map.add newfid (canonicalise Params.root wnames) !fids;
        Lwt.return (Result.Ok (Response.Walk { Response.Walk.wqids = List.rev qids }))
      | x :: xs ->
        let realpath = canonicalise Params.root (List.rev (x :: dir)) in
        qid_of_path realpath
        >>*= fun qid ->
        walk (x :: dir) (qid :: qids) xs in
    walk [] [] wnames

end
