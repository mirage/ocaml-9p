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

let example_data =
  let data = Cstruct.create 1 in
  for i = 0 to Cstruct.len data - 1 do
    Cstruct.set_char data i '\000'
  done;
  data

let qid = "1234567890123"

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
  })

let make_tag =
  let next = ref 0 in
  fun () ->
    let this = !next in
    incr next;
    match Types.Tag.of_int this with
    | Ok x -> x
    | Error _ -> failwith "Test program ran out of tags!"

let requests =
  let open Request in [
    { tag = make_tag (); payload = Version Version.({ msize = 55l; version = Types.Version.default}) };
    { tag = make_tag (); payload = Auth Auth.({ afid = 1l; uname = "hello"; aname = "there" }) };
    { tag = make_tag (); payload = Flush Flush.({ oldtag = 123 }) };
    { tag = make_tag (); payload = Attach Attach.({ fid = 3l; afid = 2l; uname = "who"; aname = "areyou?" })};
    { tag = make_tag (); payload = Walk Walk.( { fid = 4l; newfid = 5l; wnames = [ "one"; "two"; "three" ]})};
    { tag = make_tag (); payload = Open Open.( { fid = 6l; mode = 123 })};
    { tag = make_tag (); payload = Create Create.( { fid = 7l; name = "woohoo"; perm = 44l; mode = 101 })};
    { tag = make_tag (); payload = Read Read.( { fid = 8l; offset = 123456L; count = 123l })};
    { tag = make_tag (); payload = Write Write.( { fid = 9l; offset = 98765L; data = example_data })};
    { tag = make_tag (); payload = Clunk Clunk.( { fid = 10l })};
    { tag = make_tag (); payload = Remove Remove.( { fid = 11l })};
    { tag = make_tag (); payload = Stat Stat.( { fid = 12l })};
    { tag = make_tag (); payload = Wstat Wstat.( { fid = 13l; stat })};
  ]

let responses =
  let open Response in [
    { tag = make_tag (); payload = Version Version.({ msize = 55l; version = Types.Version.unknown}) };
    { tag = make_tag (); payload = Auth Auth.({ aqid = qid }) };
    { tag = make_tag (); payload = Err Err.({ ename = "it went wrong!" })};
    { tag = make_tag (); payload = Flush Flush.( () ) };
    { tag = make_tag (); payload = Attach Attach.({ qid })};
    { tag = make_tag (); payload = Walk Walk.( { wqids = [ qid; qid ] })};
    { tag = make_tag (); payload = Open Open.( { qid; iounit = 2l })};
    { tag = make_tag (); payload = Create Create.( { qid; iounit = 3l; })};
    { tag = make_tag (); payload = Read Read.( { data = example_data })};
    { tag = make_tag (); payload = Write Write.( { count = 7l })};
    { tag = make_tag (); payload = Clunk Clunk.( () )};
    { tag = make_tag (); payload = Remove Remove.( () )};
    { tag = make_tag (); payload = Stat Stat.( { stat })};
    { tag = make_tag (); payload = Wstat Wstat.( () )};
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

let print_parse_response r () =
  let open Error in
  expect_ok (
    let needed = Response.sizeof r in
    let buf = Cstruct.create needed in
    Response.write r buf
    >>= fun remaining ->
    assert_equal ~printer:string_of_int 0 (Cstruct.len remaining);
    Response.read buf
    >>= fun (r', remaining) ->
    assert_equal ~printer:string_of_int 0 (Cstruct.len remaining);
    assert_equal ~printer:(fun x -> x) (Response.to_string r) (Response.to_string r');
    return ()
  )

let tests =
  let requests =
    List.map (fun r ->
      Printf.sprintf "print then parse %s" (Request.to_string r) >:: (print_parse_request r)
    ) requests in
  let responses =
    List.map (fun r ->
      Printf.sprintf "print then parse %s" (Response.to_string r) >:: (print_parse_response r)
    ) responses in
  requests @ responses

let _ =
  let suite = "parse and print" >::: tests in
  run_test_tt_main suite
