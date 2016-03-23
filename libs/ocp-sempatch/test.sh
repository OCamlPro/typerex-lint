#!/bin/sh

ocamlbuild -cflag -ppx -cflag patch/test_patch.byte test/test_to_patch.byte
./test_to_patch.byte
