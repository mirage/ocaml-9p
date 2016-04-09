(*
 * Copyright (C) 2015 David Sheets <david.sheets@unikernel.com>
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

open Lwt
open Protocol_9p

let unix_exn_converter info exn =
  let is_unix = (info.Protocol_9p_info.version = Types.Version.unix) in
  match exn with
  | Unix.Unix_error(err, _, _) ->
    let host = match info.Protocol_9p_info.aname with
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
    Response.Err {
      Response.Err.ename = Unix.error_message err;
      errno;
    }
  | e ->
    Response.Err {
      Response.Err.ename = Printexc.to_string e;
      errno = None;
    }
