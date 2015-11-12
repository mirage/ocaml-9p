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

let example_data =
  let data = Cstruct.create 1 in
  for i = 0 to Cstruct.len data - 1 do
    Cstruct.set_char data i '\000'
  done;
  data

let qid = {
  Types.Qid.flags = [ Types.Qid.Directory ];
  id = 5L;
  version = 4l;
}

let stat = Types.Stat.({
  ty = 1;
  dev = 2l;
  qid;
  mode = Types.FileMode.make ();
  atime = 4l;
  mtime = 5l;
  length = 6L;
  name = "name";
  uid = "uid";
  gid = "gid";
  muid = "muid";
  u = Some { extension = "hello"; n_uid = 1l; n_gid = 2l; n_muid = 3l };
  })

let make_tag =
  let next = ref 0 in
  fun () ->
    let this = !next in
    incr next;
    match Types.Tag.of_int this with
    | Ok x -> x
    | Error _ -> failwith "Test program ran out of tags!"

let make_fid =
  let next = ref 0l in
  fun () ->
    let this = !next in
    next := Int32.succ !next;
    match Types.Fid.of_int32 this with
    | Ok x -> x
    | Error _ -> failwith "Test program ran out of fids!"

let openmode = Types.OpenMode.read_only

let openperm =
  let owner = [ `Read; `Write ] in
  let group = [ `Read ] in
  let other = [ `Read ] in
  Types.FileMode.make ~owner ~group ~other ()

let requests =
  let open Request in [
    { tag = make_tag (); payload = Version Version.({ msize = 55l; version = Types.Version.default}) };
    { tag = make_tag (); payload = Auth Auth.({ afid = make_fid (); uname = "hello"; aname = "there"; n_uname = Some 4l }) };
    { tag = make_tag (); payload = Flush Flush.({ oldtag = make_tag () }) };
    { tag = make_tag (); payload = Attach Attach.({ fid = make_fid (); afid = make_fid (); uname = "who"; aname = "areyou?"; n_uname = None })};
    { tag = make_tag (); payload = Walk Walk.( { fid = make_fid (); newfid = make_fid (); wnames = [ "one"; "two"; "three" ]})};
    { tag = make_tag (); payload = Open Open.( { fid = make_fid (); mode = openmode })};
    { tag = make_tag (); payload = Create Create.( { fid = make_fid (); name = "woohoo"; perm = openperm; mode = openmode; extension = Some "b 1 2"})};
    { tag = make_tag (); payload = Read Read.( { fid = make_fid (); offset = 123456L; count = 123l })};
    { tag = make_tag (); payload = Write Write.( { fid = make_fid (); offset = 98765L; data = example_data })};
    { tag = make_tag (); payload = Clunk Clunk.( { fid = make_fid () })};
    { tag = make_tag (); payload = Remove Remove.( { fid = make_fid () })};
    { tag = make_tag (); payload = Stat Stat.( { fid = make_fid () })};
    { tag = make_tag (); payload = Wstat Wstat.( { fid = make_fid (); stat })};
  ]

let responses =
  let open Response in [
    { tag = make_tag (); payload = Version Version.({ msize = 55l; version = Types.Version.unknown}) };
    { tag = make_tag (); payload = Auth Auth.({ aqid = qid }) };
    { tag = make_tag (); payload = Err Err.({ ename = "it went wrong!"; errno = None })};
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

let request: Request.t Alcotest.testable = (module Request)
let response: Response.t Alcotest.testable = (module Response)

let print_parse_request r () =
  let open Error in
  expect_ok (
    let needed = Request.sizeof r in
    let buf = Cstruct.create needed in
    Request.write r buf
    >>= fun remaining ->
    Alcotest.(check int) "write request" 0 (Cstruct.len remaining);
    Request.read buf
    >>= fun (r', remaining) ->
    Alcotest.(check int) "read request" 0 (Cstruct.len remaining);
    Alcotest.(check request) "request" r r';
    return ()
  )

let print_parse_response r () =
  let open Error in
  expect_ok (
    let needed = Response.sizeof r in
    let buf = Cstruct.create needed in
    Response.write r buf
    >>= fun remaining ->
    Alcotest.(check int) "write response" 0 (Cstruct.len remaining);
    Response.read buf
    >>= fun (r', remaining) ->
    Alcotest.(check int) "read respsonse" 0 (Cstruct.len remaining);
    Alcotest.(check response) "response" r r';
    return ()
  )

let test_requests = List.map (fun r ->
    Fmt.strf "print then parse %a" Request.pp r,
    `Quick,
    print_parse_request r
  ) requests

let test_responses = List.map (fun r ->
    Fmt.strf "print then parse %a" Response.pp r,
    `Quick,
    print_parse_response r
  ) responses

let tests = [
  "request" , test_requests;
  "response", test_responses;
]

let () = Alcotest.run "9p" tests
