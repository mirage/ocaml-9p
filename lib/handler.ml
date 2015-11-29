(*
 * Copyright (C) 2015 David Sheets <david.sheets@unikernel.com>
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

(** Given a traditional file system, construct a handler for 9p messages. *)

open Result
open Lwt.Infix

module Make(Filesystem : Filesystem.S) = struct
  let receive_cb info ~cancel =
    let is_unix = (info.Server.version = Types.Version.unix) in
    let adjust_errno err =
      if not is_unix then { err with Response.Err.errno = None }
      else match err.Response.Err.errno with
      | Some _ -> err
      | None -> {err with Response.Err.errno = Some 0l} in
    let wrap fn x result =
      fn info ~cancel x >|= function
      | Ok response -> Ok (result response)
      | Error err -> Ok (Response.Err (adjust_errno err)) in
    Request.(function
    | Attach x -> wrap Filesystem.attach x (fun x -> Response.Attach x)
    | Walk x   -> wrap Filesystem.walk   x (fun x -> Response.Walk x)
    | Open x   -> wrap Filesystem.open_  x (fun x -> Response.Open x)
    | Read x   -> wrap Filesystem.read   x (fun x -> Response.Read x)
    | Clunk x  -> wrap Filesystem.clunk  x (fun x -> Response.Clunk x)
    | Stat x   -> wrap Filesystem.stat   x (fun x -> Response.Stat x)
    | Create x -> wrap Filesystem.create x (fun x -> Response.Create x)
    | Write x  -> wrap Filesystem.write  x (fun x -> Response.Write x)
    | Remove x -> wrap Filesystem.remove x (fun x -> Response.Remove x)
    | Wstat x  -> wrap Filesystem.wstat  x (fun x -> Response.Wstat x)
    | Version _ | Auth _ | Flush _ ->
        let err = {Response.Err.ename = "Function not implemented"; errno = None} in
        Lwt.return (Result.Ok (Response.Err (adjust_errno err)))
  )
end
