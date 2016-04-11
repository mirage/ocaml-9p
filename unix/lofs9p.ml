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

(* This is an implementation of a POSIX loopback filesystem on top of
   a POSIX syscall interface which is expected to be consumed by a
   POSIX client. *)

open Protocol_9p
open Infix
open Lwt
open Result

let (/) = Filename.concat

(* Turn a "fatal" 9p error into a "safe" error response to the client *)
let errors_to_client = function
  | Error (`Msg msg) -> Error {Response.Err.ename = msg; errno = None}
  | Ok _ as ok -> ok

(* We need to associate files with Qids with unique ids and versions *)
let qid_of_path realpath =
  Lwt_unix.LargeFile.lstat realpath
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

let bad_fid = Lwt.return (Response.error "bad fid")
let fid_in_use = Lwt.return (Response.error "fid already in use")

type t = {
  root: string list;
}

let make root = { root }

  module Path = struct
    type t = {
      segments : string list;
    }

    let mount_depth t = List.length t.root
    let root = { segments = []; }

    let realroot t = match t.root with
      | [] -> List.tl (Stringext.split (Unix.getcwd ()) ~on:'/')
      | ""::path -> path
      | _::_ ->
        (List.tl (Stringext.split (Unix.getcwd ()) ~on:'/')) @ t.root

    (* Convert from a segmented path to real paths *)
    let realpath =
      let add_segment buf segment =
        Buffer.add_char buf '/';
        Buffer.add_string buf segment
      in
      fun t { segments } ->
        let buf = Buffer.create (8 * (mount_depth t + List.length segments)) in
        List.iter (add_segment buf) (realroot t);
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

    let stat t info path =
      let realpath = realpath t path in
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
      ( if info.Protocol_9p_info.version = Types.Version.unix then begin
          ( match stats.LargeFile.st_kind with
            | S_LNK ->
              Lwt_unix.readlink realpath
            | S_BLK ->
              return "b 0 0" (* FIXME: need major, minor *)
            | S_CHR ->
              return "c 0 0" (* FIXME: need major, minor *)
            | _ ->
              return "" )
          >>= fun extension ->
          let n_uid = Int32.of_int stats.LargeFile.st_uid in
          let n_gid = Int32.of_int stats.LargeFile.st_gid in
          let n_muid = n_uid in
          return (Some { Types.Stat.extension; n_uid; n_gid; n_muid })
        end else return None )
      >>= fun u ->
      let stat =
        Types.Stat.make ~name ~qid ~mode ~length ~atime ~mtime ~uid ~gid ?u ()
      in
      Lwt.return (Result.Ok stat)

  end

  (* We need to remember the mapping of Fid to path *)

  module Resource = struct
    type handle =
      | File of Lwt_unix.file_descr
      | Dir of Lwt_unix.dir_handle

    type t = {
      path: Path.t;
      handle: handle option;
      m: Lwt_mutex.t;
    }

    let of_path path = { path; handle = None; m = Lwt_mutex.create () }
    let of_fd path fd = { path; handle = Some (File fd); m = Lwt_mutex.create () }
    let of_dir t path =
      Lwt_unix.opendir (Path.realpath t path)
      >>= fun h ->
      Lwt.return { path; handle = Some (Dir h); m = Lwt_mutex.create () }

    let with_lock t f = Lwt_mutex.with_lock t.m f

    let is_open t = t.handle <> None

    let close t = match t.handle with
      | None -> return_unit
      | Some (File f) -> Lwt_unix.close f
      | Some (Dir d) -> Lwt_unix.closedir d
  end

  type connection = {
    t: t;
    info: Protocol_9p_info.t;
    fids: Resource.t Types.Fid.Map.t ref;
  }

  let connect t info  =
    let fids = ref Types.Fid.Map.empty in
    { t; info; fids }

  let path_of_fid connection fid =
    if fid = connection.info.Protocol_9p_info.root
    then Path.root
    else (Types.Fid.Map.find fid !(connection.fids)).Resource.path

  let read connection ~cancel { Request.Read.fid; offset; count } =
    (* The client can requests a count which is larger than the negotiated msize *)
    let max_count = Int32.(sub (sub connection.info.Protocol_9p_info.msize (of_int Response.sizeof_header)) (of_int Response.Read.sizeof_header)) in
    let count = min max_count count in
    match path_of_fid connection fid with
    | exception Not_found -> bad_fid
    | path ->
      let buffer = Lwt_bytes.create (Int32.to_int count) in
      let resource = Types.Fid.Map.find fid !(connection.fids) in
      begin match resource.Resource.handle with
      | None -> bad_fid
      | Some (Resource.Dir h) ->
        let t =
          Resource.with_lock resource
            (fun () ->
              Lwt_unix.rewinddir h
              >>= fun () ->
              Lwt_unix.readdir_n h 1024
            )
          >>= fun xs ->
          let rec write off rest = function
            | [] -> Lwt.return (Result.Ok off)
            | x :: xs ->
              Path.(stat connection.t connection.info (append path x))
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
          let data = Cstruct.sub rest 0 Int64.(to_int (max 0L (sub offset' offset))) in
          Lwt.return (Result.Ok { Response.Read.data }) in
        t >>= fun x -> Lwt.return (errors_to_client x)
      | Some (Resource.File fd) ->
        Resource.with_lock resource
          (fun () ->
            Lwt_unix.LargeFile.lseek fd offset Lwt_unix.SEEK_SET
            >>= fun _ ->
            Lwt_bytes.read fd buffer 0 (Lwt_bytes.length buffer)
          )
        >>= fun n ->
        let data = Cstruct.(sub (of_bigarray buffer) 0 n) in
        Lwt.return (Result.Ok { Response.Read.data })
      end

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

  let open_ connection ~cancel { Request.Open.fid; mode } =
    match path_of_fid connection fid with
    | exception Not_found -> bad_fid
    | path ->
      let resource = Types.Fid.Map.find fid !(connection.fids) in
      if Resource.is_open resource
      then bad_fid
      else begin
        let realpath = Path.realpath connection.t path in
        qid_of_path realpath
        >>*= fun qid ->
        ( if List.mem Types.Qid.Directory qid.Types.Qid.flags then begin
            Resource.of_dir connection.t resource.Resource.path
            >>= fun resource ->
            connection.fids := Types.Fid.Map.add fid resource !(connection.fids);
            return_unit
          end else begin
            Lwt_unix.openfile realpath (flags_of_mode mode) 0
            >>= fun fd ->
            (* What is the point of seeking to the end of the file when
               read and write have offset arguments? *)
            ( if mode.Types.OpenMode.append
              then Lwt_unix.LargeFile.lseek fd 0L Lwt_unix.SEEK_END
              else Lwt.return 0L
            ) >>= fun _ ->
            let resource = Resource.of_fd resource.Resource.path fd in
            connection.fids := Types.Fid.Map.add fid resource !(connection.fids);
            return_unit
          end )
        >>= fun () ->
        Lwt.return (Result.Ok { Response.Open.qid; iounit = 0_l })
      end

  let clunk connection ~cancel { Request.Clunk.fid } =
    if not (Types.Fid.Map.mem fid !(connection.fids))
    then bad_fid
    else begin
      let resource = Types.Fid.Map.find fid !(connection.fids) in
      connection.fids := Types.Fid.Map.remove fid !(connection.fids);
      Resource.close resource
      >>= fun () ->
      Lwt.return (Result.Ok ())
    end

  let disconnect connection info =
    let resources = Types.Fid.Map.fold (fun _ resource acc -> resource :: acc) !(connection.fids) [] in
    Lwt_list.iter_s Resource.close resources

  let walk connection ~cancel { Request.Walk.fid; newfid; wnames } =
    let rec walk dir qids = function
      | [] ->
        connection.fids := Types.Fid.Map.add newfid (Resource.of_path dir) !(connection.fids);
        Lwt.return (Result.Ok {
          Response.Walk.wqids = List.rev qids;
        })
      | x :: xs ->
        let here = Path.append dir x in
        let realpath = Path.realpath connection.t here in
        qid_of_path realpath
        >>*= fun qid ->
        walk here (qid :: qids) xs
    in
    if Types.Fid.Map.mem newfid !(connection.fids) && (fid <> newfid)
    then fid_in_use
    else
      match path_of_fid connection fid with
      | exception Not_found -> bad_fid
      | path ->
        walk path [] wnames

  let attach connection ~cancel { Request.Attach.fid } =
    (* bind the fid as another root *)
    connection.fids := Types.Fid.Map.add fid (Resource.of_path Path.root) !(connection.fids);
    let realpath = Path.realpath connection.t Path.root in
    qid_of_path realpath
    >>*= fun qid ->
    Lwt.return (Result.Ok { Response.Attach.qid })

  let stat connection ~cancel { Request.Stat.fid } =
    match path_of_fid connection fid with
    | exception Not_found -> bad_fid
    | path ->
      Path.stat connection.t connection.info path
      >>*= fun stat ->
      Lwt.return (Result.Ok { Response.Stat.stat })

  let bad_create msg = Lwt.return (Response.error "can't create %s" msg)

  let create connection ~cancel { Request.Create.fid; name; perm; mode; extension } =
    match path_of_fid connection fid with
    | exception Not_found -> bad_fid
    | path ->
      let realpath = (Path.realpath connection.t path) / name in
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
          Resource.of_dir connection.t (Path.append path name)
          >>= fun resource ->
          connection.fids := Types.Fid.Map.add fid resource !(connection.fids);
          Lwt.return (Result.Ok {
            Response.Create.qid;
            iounit = 0_l;
          })
      )
      else if perm.Types.FileMode.is_symlink
      then (
        match extension with
        | Some target ->
          Lwt_unix.symlink target realpath
          >>= fun () ->
          qid_of_path realpath
          >>*= fun qid ->
          connection.fids := Types.Fid.Map.add fid (Resource.of_path (Path.append path name)) !(connection.fids);
          Lwt.return (Result.Ok {
            Response.Create.qid;
            iounit = 0_l;
          })
        | None ->
          Lwt.return (Response.error "creating symlinks requires 9p2000.u extension")
      )
      else if perm.Types.FileMode.is_hardlink
      then (
        match extension with
        | Some fid_string ->
          (* Linux puts a newline on the end of the string for some reason *)
          let lookup_fid x =
            match Types.Fid.of_int32 @@ Int32.of_string @@ String.trim fid_string with
            | Ok fid -> path_of_fid connection fid
            | _ -> raise Not_found in
          begin match lookup_fid fid_string with
          | exception Not_found -> bad_fid
          | target ->
            Lwt_unix.link (Path.realpath connection.t target) realpath
            >>= fun () ->
            qid_of_path realpath
            >>*= fun qid ->
            connection.fids := Types.Fid.Map.add fid (Resource.of_path (Path.append path name)) !(connection.fids);
            Lwt.return (Result.Ok {
              Response.Create.qid;
              iounit = 0_l;
            })
          end
        | None ->
          Lwt.return (Response.error "creating hardlinks requires 9p2000.u extension")
        )
      else
        let flags = flags_of_mode mode in
        Lwt_unix.(openfile realpath (O_CREAT :: O_EXCL :: flags) perms)
        >>= fun fd ->
        qid_of_path realpath
        >>*= fun qid ->
        connection.fids := Types.Fid.Map.add fid (Resource.of_fd (Path.append path name) fd) !(connection.fids);
        Lwt.return (Result.Ok {
          Response.Create.qid;
          iounit = 0_l;
        })

  let write connection ~cancel { Request.Write.fid; offset; data } =
    match path_of_fid connection fid with
    | exception Not_found -> bad_fid
    | path ->
      let resource = Types.Fid.Map.find fid !(connection.fids) in
      begin match resource.Resource.handle with
      | None -> bad_fid
      | Some (Resource.Dir _) -> bad_fid
      | Some (Resource.File fd) ->
        Resource.with_lock resource
          (fun () ->
            Lwt_unix.LargeFile.lseek fd offset Lwt_unix.SEEK_SET
            >>= fun _cursor ->
            let len = Cstruct.len data in
            Lwt_unix.write fd (Bytes.of_string (Cstruct.to_string data)) 0 len
          )
        >>= fun written ->
        let count = Int32.of_int written in
        Lwt.return (Result.Ok { Response.Write.count })
      end

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
    return_unit

  let set_length path length =
    Lwt_unix.LargeFile.truncate path length

  let rename_local path name =
    let newpath = Filename.((dirname path) / name) in
    Lwt_unix.rename path newpath

  (* Does not guarantee atomicity of general wstat messages like plan
     9 requires! Luckily, both sides are actually POSIX so we're
     probably fine. *)
  let wstat connection ~cancel { Request.Wstat.fid; stat } =
    match path_of_fid connection fid with
    | exception Not_found -> bad_fid
    | path ->
      let realpath = Path.realpath connection.t path in
      let {
        Types.Stat.ty; dev; qid;
        mode; atime; mtime; length; name;
      } = stat in
      (* we just ignore any attempts to change muid, uid, gid *)
      if not (Types.Int16.is_any ty)
      then Lwt.return (Response.error "wstat can't change type")
      else if not (Types.Int32.is_any dev)
      then Lwt.return (Response.error "wstat can't change dev")
      else if not (Types.Qid.is_any qid)
      then Lwt.return (Response.error "wstat can't change qid")
      else begin
        (* TODO: check permissions *)
        (if not (Types.FileMode.is_any mode)
         then set_mode realpath mode
         else return_unit
        ) >>= fun () ->
        (if not (Types.Int32.is_any atime && Types.Int32.is_any mtime)
         then set_times realpath atime mtime
         else return_unit
        ) >>= fun () ->
        (if not (Types.Int64.is_any length)
         then set_length realpath length
         else return_unit
        ) >>= fun () ->
        (if name <> ""
         then rename_local realpath name
         else return_unit
        ) >>= fun () ->
        Lwt.return (Result.Ok ())
      end

  let remove connection ~cancel { Request.Remove.fid } =
    match path_of_fid connection fid with
    | exception Not_found -> bad_fid
    | path ->
      (* Always clunk the fid, even if remove fails *)
      let realpath = Path.realpath connection.t path in
      clunk connection ~cancel { Request.Clunk.fid }
      >>*= fun () ->
      let rec loop () =
        Lwt.catch
          (fun () ->
            Lwt_unix.LargeFile.lstat realpath
            >>= fun stats ->
            ( if stats.Lwt_unix.LargeFile.st_kind = Lwt_unix.S_DIR
              then Lwt_unix.rmdir else Lwt_unix.unlink ) realpath
          ) (function
            | Unix.Unix_error((Unix.EISDIR | Unix.ENOTDIR), _, _) ->
              loop ()
            | e ->
              fail e) in
      loop ()
      >>= fun () ->
      Lwt.return (Result.Ok ())
