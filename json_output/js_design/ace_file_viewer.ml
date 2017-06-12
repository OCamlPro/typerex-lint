open Ace
open Lint_warning_types
open Lint_warning_json

(*******)
let find_component id =
  match Js_utils.Manip.by_id id with
  | Some div -> div
  | None -> failwith ("Cannot find id " ^ id)
(*******)
		     
let doc = Dom_html.document

let theme =
  "monokai"

let font_size =
  14

let context_line_number =
  3

let array_joining join arr =
  let hd = arr.(0) in
  let tl = Array.sub arr 1 ((Array.length arr)-1) in
  Array.fold_left begin fun acc line ->
    acc ^ join ^ line 
  end hd tl  
  
let code_viewer_register_warning ace warning =
  let begin_line, begin_col, end_line, end_col =
    match file_loc_of_loc warning.loc with
    | Some (Floc_line line) ->
       line, 0, line, String.length (Ace.get_line ace (line - 1)) 
    | Some (Floc_lines_cols (bline, bcol, eline, ecol)) ->
       bline, bcol, eline, ecol
    | None ->
       failwith "no location for this warning"
  in
  let begin_context = min (begin_line - 1) context_line_number in
  let end_context = min ((Ace.get_length ace) - end_line) context_line_number in
  let begin_with_context = begin_line - begin_context in
  let end_with_context = end_line + end_context in
  (***** verifier si warning au limite *****)
  let loc = {
    loc_start = begin_context + 1, begin_col;
    loc_end = begin_context + end_line - begin_line + 1, end_col
  } in
  (*****************************************)
  let content =
    array_joining
      "\n"
      (Ace.get_lines ace (begin_with_context - 1) (end_with_context - 1))
  in
  Ace.set_value ace content;
  Ace.clear_selection ace;
  Ace.set_option ace "firstLineNumber" begin_with_context;
  Ace.add_marker ace Ace.Warning loc;
  Ace.set_annotation ace Ace.Warning warning.output loc
  
let create_ocaml_code_viewer div warning =
  let ace = Ace.create_editor div in
  Ace.set_mode ace "ace/mode/ocaml";
  Ace.set_theme ace ("ace/theme/" ^ theme);
  Ace.set_font_size ace font_size;
  Ace.set_read_only ace true;
  code_viewer_register_warning ace warning

let set_div_ocaml_code_view warning =
  let code_div = find_component "code" (***** code id dans lint_web ****) in
  create_ocaml_code_viewer (Tyxml_js.To_dom.of_div code_div) warning

(*** utils function ***)
let json_from_js_var var =
  let (str : Js.js_string Js.t) = Js.Unsafe.variable var in
  Yojson.Basic.from_string (Js.to_string str)
(**********************)
			   
let onload _ =
  let json = json_from_js_var "json" in
  let id =
    try int_of_string (Url.Current.get_fragment ()) with
    | Failure _ -> failwith "invalid id"
  in
  let entry =
    try
      json
      |> database_warning_entries_of_json
      |> List.find (fun entry -> entry.id = id)
    with
    | Not_found -> failwith "invalid id"
  in
  set_div_ocaml_code_view entry.warning_result;
  Js._true

let () =
  Dom_html.window##onload <- Dom_html.handler onload
