open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json

type file_loc =
  | Floc_line of int
  | Floc_lines_cols of int * int * int * int

let file_loc_of_loc loc =
  let open Location in
  let open Lexing in
  let col_of_pos pos = pos.pos_cnum - pos.pos_bol in
  if not loc.loc_ghost then
    Some (Floc_lines_cols (
	loc.loc_start.pos_lnum,
	col_of_pos loc.loc_start,
	loc.loc_end.pos_lnum,
	col_of_pos loc.loc_end
      ))
  else if loc.loc_start.pos_lnum != 0 then
    Some (Floc_line loc.loc_start.pos_lnum)
  else
    None
       
let array_joining join arr =
  let hd = arr.(0) in
  let tl = Array.sub arr 1 ((Array.length arr) - 1) in
  Array.fold_left begin fun acc line ->
    acc ^ join ^ line 
  end hd tl 
       
let warning_href warning_entry =
    (Lint_web.web_static_gen_file warning_entry.hash)
    ^ ".html#"
    ^ (string_of_int warning_entry.id)

let json_from_js_var var =
  let (str : Js.js_string Js.t) = Js.Unsafe.variable var in
  Yojson.Basic.from_string (Js.to_string str)
       
let find_plugin_entry warning_entry plugins_entries =
  List.find begin fun plugin_entry ->
    warning_entry.plugin_name = plugin_entry.plugin_entry_plugin_name
    && warning_entry.linter_name = plugin_entry.plugin_entry_linter_name
  end plugins_entries
