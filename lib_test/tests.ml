(*
 * Copyright (c) 2015 David Scott <dave@recoil.org>
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
open Result
open OUnit

let requests =
  let open Request in [
    { tag = 11; payload = Version Version.({ msize = 55l; version = "some version"}) }
  ]

let expect_ok = function
  | Ok x -> x
  | Error (`Msg m) -> failwith m

let print_parse_request r () =
  let open Error in
  expect_ok (
    let needed = Request.sizeof r in
    let buf = Cstruct.create needed in
    Request.write r buf
    >>= fun remaining ->
    assert_equal ~printer:string_of_int 0 (Cstruct.len remaining);
    Request.read buf
    >>= fun (r', remaining) ->
    assert_equal ~printer:string_of_int 0 (Cstruct.len remaining);
    assert_equal ~printer:Request.to_string r r';
    return ()
  )

let tests =
  List.map (fun r ->
    Printf.sprintf "print then parse %s" (Request.to_string r) >:: (print_parse_request r)
  ) requests

let _ =
  let suite = "parse and print" >::: tests in
  run_test_tt_main suite
