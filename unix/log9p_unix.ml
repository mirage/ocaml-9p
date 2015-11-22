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

module Stdout = struct
  let print_debug = ref false
  let debug fmt =
    Fmt.kstrf (fun s -> if !print_debug then print_endline s) fmt
  let info  fmt = Fmt.kstrf (fun s -> print_endline s) fmt
  let error fmt = Fmt.kstrf (fun s -> print_endline s) fmt
end

module StdoutPrefix(Config : sig val prefix : string end) = struct
  let print_debug = ref false

  let print = Printf.printf "%s %s\n%!" Config.prefix

  let debug fmt =
    Printf.ksprintf (fun s -> if !print_debug then print s) fmt
  let info  fmt = Printf.ksprintf (fun s -> print s) fmt
  let error fmt = Printf.ksprintf (fun s -> print s) fmt
end
