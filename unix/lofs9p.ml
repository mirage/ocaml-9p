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

let (/) = Filename.concat

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
    type t = {
      segments : string list;
    }

    let mount_depth = List.length Params.root
    let root = { segments = []; }

    let realroot = match Params.root with
      | [] -> List.tl (Stringext.split (Unix.getcwd ()) ~on:'/')
      | ""::path -> path
      | _::_ ->
        (List.tl (Stringext.split (Unix.getcwd ()) ~on:'/')) @ Params.root

    (* Convert from a segmented path to real paths *)
    let realpath =
      let add_segment buf segment =
        Buffer.add_char buf '/';
        Buffer.add_string buf segment
      in
      fun { segments } ->
        let buf = Buffer.create (8 * (mount_depth + List.length segments)) in
        List.iter (add_segment buf) realroot;
        List.iter (add_segment buf) (List.rev segments);
        Buffer.contents buf

    let append path node = { segments = node::path.segments }

    let of_segments segments = { segments = List.rev segments }

    let perms_of_code code =
      match code land 7 with
      | 1 -> [ `Execute ]
      | 2 -> [ `Write ]
      | 3 -> [ `Execute; `Write ]
      | 4 -> [ `Read ]
      | 5 -> [ `Execute; `Read ]
      | 6 -> [ `Write; `Read ]
      | 7 -> [ `Execute; `Write; `Read ]
      | _ -> []

    let stat path =
      let realpath = realpath path in
      let open Lwt_unix in
      LargeFile.lstat realpath
      >>= fun stats ->
      qid_of_path realpath
      >>*= fun qid ->
      let { LargeFile.st_perm } = stats in
      let owner = perms_of_code (st_perm lsr 6) in
      let group = perms_of_code (st_perm lsr 3) in
      let other = perms_of_code st_perm in
      let is_device = List.mem stats.LargeFile.st_kind [ S_BLK; S_CHR ] in
      let mode = Types.FileMode.make
          ~owner ~group ~other
          ~is_directory:(stats.LargeFile.st_kind = S_DIR)
          ~is_device
          ~is_symlink:(stats.LargeFile.st_kind = S_LNK) () in
      let name = match path.segments with [] -> "/" | node::_ -> node in
      let length = stats.LargeFile.st_size in
      let atime = Int32.of_float stats.LargeFile.st_atime in
      let mtime = Int32.of_float stats.LargeFile.st_mtime in
      let uid = string_of_int stats.LargeFile.st_uid in
      let gid = string_of_int stats.LargeFile.st_gid in
      let stat =
        Types.Stat.make ~name ~qid ~mode ~length ~atime ~mtime ~uid ~gid ()
      in
      Lwt.return (Result.Ok stat)

  end

  (* We need to remember the mapping of Fid to path *)
  let fids : Path.t Types.Fid.Map.t ref = ref Types.Fid.Map.empty

  let path_of_fid info fid =
    if fid = info.Server.root
    then Path.root
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
            Path.(stat (append path x))
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
        fids := Types.Fid.Map.add newfid dir !fids;
        Lwt.return (Result.Ok (Response.Walk {
          Response.Walk.wqids = List.rev qids;
        }))
      | x :: xs ->
        let here = Path.append dir x in
        let realpath = Path.realpath here in
        qid_of_path realpath
        >>*= fun qid ->
        walk here (qid :: qids) xs
    in
    match path_of_fid info fid with
    | exception Not_found -> bad_fid
    | path ->
      walk path [] wnames

  let stat info { Request.Stat.fid } =
    match path_of_fid info fid with
    | exception Not_found -> bad_fid
    | path ->
      Path.stat path
      >>*= fun stat ->
      Lwt.return (Result.Ok Response.(Stat { Stat.stat }))

  let bad_create msg = Lwt.return (Result.Error (`Msg ("can't create "^msg)))

  let flags_of_mode mode =
    let open Types.OpenMode in
    let flags = match mode.io with
      | Read -> [ Lwt_unix.O_RDONLY ]
      | Write -> [ Lwt_unix.O_WRONLY ]
      | ReadWrite -> [ Lwt_unix.O_RDWR ]
      | Exec -> []
    in
    (* TODO: support ORCLOSE? *)
    if mode.truncate then Lwt_unix.O_TRUNC :: flags else flags

  let create info { Request.Create.fid; name; perm; mode } =
    match path_of_fid info fid with
    | exception Not_found -> bad_fid
    | path ->
      let realpath = (Path.realpath path) / name in
      let perms = Int32.to_int (Types.FileMode.nonet_of_permissions perm) in
      if perm.Types.FileMode.is_directory
      then (
        if mode.Types.OpenMode.rclose
        then bad_create "directory with ORCLOSE"
        else if Types.OpenMode.(mode.io = Write)
        then bad_create "directory with OWRITE"
        else if Types.OpenMode.(mode.io = ReadWrite)
        then bad_create "directory with ORDWR"
        else if mode.Types.OpenMode.truncate
        then bad_create "directory with OTRUNC"
        else
          Lwt_unix.mkdir realpath perms
          >>= fun () ->
          qid_of_path realpath
          >>*= fun qid ->
          Lwt.return (Result.Ok (Response.Create {
            Response.Create.qid;
            iounit = 512l;
          }))
      )
      else
        let flags = flags_of_mode mode in
        Lwt_unix.(openfile realpath (O_CREAT :: O_EXCL :: flags) perms)
        >>= fun fd ->
        Lwt_unix.close fd
        >>= fun () ->
        qid_of_path realpath
        >>*= fun qid ->
        Lwt.return (Result.Ok (Response.Create {
          Response.Create.qid;
          iounit = 512l;
        }))

  let write info { Request.Write.fid; offset; data } =
    match path_of_fid info fid with
    | exception Not_found -> bad_fid
    | path ->
      let realpath = Path.realpath path in
      Lwt_unix.(openfile realpath [O_WRONLY] 0o600)
      >>= fun fd ->
      Lwt_unix.LargeFile.lseek fd offset Lwt_unix.SEEK_SET
      >>= fun _cursor ->
      let len = Cstruct.len data in
      Lwt_unix.write fd (Bytes.of_string (Cstruct.to_string data)) 0 len
      >>= fun written ->
      Lwt_unix.close fd
      >>= fun () ->
      let count = Int32.of_int written in
      Lwt.return (Result.Ok (Response.Write { Response.Write.count }))

  let set_mode path mode =
    (* TODO: handle more than just permissions *)
    let perms = Int32.to_int (Types.FileMode.nonet_of_permissions mode) in
    Lwt_unix.chmod path perms

  let set_times path atime mtime =
    (* TODO: this can block on Unix.utimes and we shouldn't but
       Lwt_unix does not yet wrap Unix.utimes. *)
    (* NOTE: The behavior of this is slightly wrong as 'any' means
       "don't change" but 0 means "set to now". *)
    let atime = if Types.Int32.is_any atime then 0.0 else Int32.to_float atime in
    let mtime = if Types.Int32.is_any mtime then 0.0 else Int32.to_float mtime in
    Unix.utimes path atime mtime;
    Lwt.return ()

  let set_length path length =
    Lwt_unix.LargeFile.truncate path length

  let rename_local path name =
    let newpath = Filename.((dirname path) / name) in
    Lwt_unix.rename path newpath

  (* Does not guarantee atomicity of general wstat messages like plan
     9 requires! Luckily, both sides are actually POSIX so we're
     probably fine. *)
  let wstat info { Request.Wstat.fid; stat } =
    match path_of_fid info fid with
    | exception Not_found -> bad_fid
    | path ->
      let realpath = Path.realpath path in
      let {
        Types.Stat.ty; dev; qid;
        mode; atime; mtime; length; name;
      } = stat in
      (* we just ignore any attempts to change muid, uid, gid *)
      if not (Types.Int16.is_any ty)
      then Lwt.return (Result.Error (`Msg "wstat can't change type"))
      else if not (Types.Int32.is_any dev)
      then Lwt.return (Result.Error (`Msg "wstat can't change dev"))
      else if not (Types.Qid.is_any qid)
      then Lwt.return (Result.Error (`Msg "wstat can't change qid"))
      else begin
        (* TODO: check permissions *)
        (if not (Types.FileMode.is_any mode)
         then set_mode realpath mode
         else Lwt.return ()
        ) >>= fun () ->
        (if not (Types.Int32.is_any atime && Types.Int32.is_any mtime)
         then set_times realpath atime mtime
         else Lwt.return ()
        ) >>= fun () ->
        (if not (Types.Int64.is_any length)
         then set_length realpath length
         else Lwt.return ()
        ) >>= fun () ->
        (if name <> ""
         then rename_local realpath name
         else Lwt.return ()
        ) >>= fun () ->
        Lwt.return (Result.Ok (Response.Wstat ()))
      end

  let remove info { Request.Remove.fid } =
    match path_of_fid info fid with
    | exception Not_found -> bad_fid
    | path ->
      let realpath = Path.realpath path in
      Lwt_unix.unlink realpath
      >>= fun () ->
      Lwt.return (Result.Ok (Response.Remove ()))
end
