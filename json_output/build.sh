#!/bin/sh

JS_PATH=js_design
JS=simple_table
EXEC=static/js/json_output

ocamlfind ocamlc -package js_of_ocaml -package yojson -package compiler-libs.common -package js_of_ocaml.syntax -syntax camlp4o -linkpkg -o $EXEC.byte $JS_PATH/$JS.ml
js_of_ocaml +weak.js +toplevel.js $EXEC.byte -o $EXEC.js --pretty --noinline
rm $EXEC.byte $JS_PATH/$JS.cmi $JS_PATH/$JS.cmo
