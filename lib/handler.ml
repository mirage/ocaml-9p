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

module Make(Filesystem : Filesystem.S) = struct
  
  let receive_cb info = Request.(function
    | Walk walk -> Filesystem.walk info walk
    | Open open_ -> Filesystem.open_ info open_
    | Read read -> Filesystem.read info read
    | Clunk clunk -> Filesystem.clunk info clunk
    | Stat stat -> Filesystem.stat info stat
    | Create create -> Filesystem.create info create
    | Write write -> Filesystem.write info write
    | Remove remove -> Filesystem.remove info remove
    | Wstat wstat -> Filesystem.wstat info wstat
    | Version _ | Auth _ | Flush _ | Attach _ ->
      Lwt.return (Result.Ok (Response.Err {
        Response.Err.ename = "not implemented";
        errno = None;
      }))
  )
end
