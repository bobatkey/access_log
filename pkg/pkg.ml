#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"

open Topkg

let () =
  Pkg.describe "access_log" @@ fun c ->
  Ok [ Pkg.mllib "src/access_log.mllib"
     ]
