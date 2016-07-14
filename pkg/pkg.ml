#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let term = Conf.with_pkg "lambda-term"
let opams = [Pkg.opam_file ~lint_deps_excluding:None "opam"]

let () =
  Pkg.describe ~opams "protocol-9p" @@ fun c ->
  let term = Conf.value c term in
  Ok [
    Pkg.mllib "lib/protocol-9p.mllib";
    Pkg.mllib "unix/protocol-9p-unix.mllib";
    Pkg.bin ~cond:term "src/main" ~dst:"o9p";
    Pkg.test "lib_test/lofs_test";
    Pkg.test "lib_test/tests";
  ]
