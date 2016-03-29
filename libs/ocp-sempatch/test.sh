#!/bin/sh

ocamlbuild -cflag -ppx -cflag "src/patch_engine.byte patch/test_patch.ml" test/test_to_patch.byte
./test_to_patch.byte
