#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mirage-vnetif" @@ fun c ->
  Ok [ Pkg.mllib "src/mirage-vnetif.mllib" ]
