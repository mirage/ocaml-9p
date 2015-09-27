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
  let data = Cstruct.create 1 in
  for i = 0 to Cstruct.len data - 1 do
    Cstruct.set_char data i '\000'
  done;
  let qid = "1234567890123" in
  let stat = Types.Stat.({
    ty = 1;
    dev = 2l;
    qid;
    mode = 3l;
    atime = 4l;
    mtime = 5l;
    length = 6L;
    name = "name";
    uid = "uid";
    gid = "gid";
    muid = "muid";
    }) in
  let open Request in [
    { tag = 11; payload = Version Version.({ msize = 55l; version = "some version"}) };
    { tag = 12; payload = Auth Auth.({ afid = 1l; uname = "hello"; aname = "there" }) };
    { tag = 13; payload = Flush Flush.({ oldtag = 123 }) };
    { tag = 14; payload = Attach Attach.({ fid = 3l; afid = 2l; uname = "who"; aname = "areyou?" })};
    { tag = 15; payload = Walk Walk.( { fid = 4l; newfid = 5l; wnames = [ "one"; "two"; "three" ]})};
    { tag = 16; payload = Open Open.( { fid = 6l; mode = 123 })};
    { tag = 17; payload = Create Create.( { fid = 7l; name = "woohoo"; perm = 44l; mode = 101 })};
    { tag = 18; payload = Read Read.( { fid = 8l; offset = 123456L; count = 123l })};
    { tag = 19; payload = Write Write.( { fid = 9l; offset = 98765L; data })};
    { tag = 20; payload = Clunk Clunk.( { fid = 10l })};
    { tag = 21; payload = Remove Remove.( { fid = 11l })};
    { tag = 22; payload = Stat Stat.( { fid = 12l })};
    { tag = 23; payload = Wstat Wstat.( { fid = 13l; stat })};
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
    assert_equal ~printer:(fun x -> x) (Request.to_string r) (Request.to_string r');
    return ()
  )

let tests =
  List.map (fun r ->
    Printf.sprintf "print then parse %s" (Request.to_string r) >:: (print_parse_request r)
  ) requests

let _ =
  let suite = "parse and print" >::: tests in
  run_test_tt_main suite
