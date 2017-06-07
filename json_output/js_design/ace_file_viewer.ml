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
  
let code_viewer_register_warnings ace warning =
  let char_column pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
  let begin_line = warning.loc.Location.loc_start.Lexing.pos_lnum in
  let end_line = warning.loc.Location.loc_end.Lexing.pos_lnum in
  let begin_context = min (begin_line - 1) context_line_number in
  let end_context = min ((Ace.get_length ace) - end_line) context_line_number in
  let begin_with_context = begin_line - begin_context in
  let end_with_context = end_line + end_context in
  let begin_char_column = char_column warning.loc.Location.loc_start in
  let end_char_column = char_column warning.loc.Location.loc_end in
  (***** verifier si warning au limite *****)
  let loc = {
    loc_start = begin_context + 1,
		begin_char_column;
    loc_end = begin_context + end_line - begin_line + 1,
	      end_char_column
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
  code_viewer_register_warnings ace warning

let set_div_ocaml_code_view warning =
  let code_div = find_component "code" (***** code id dans lint_web ****) in
  create_ocaml_code_viewer (Tyxml_js.To_dom.of_div code_div) warning
    
let onload _ =
  let (str_js: Js.js_string Js.t) =
    Js.Unsafe.variable "json" (* lint_web.var_json_name *)
  in
  let json =
    Yojson.Basic.from_string (Js.to_string str_js)
  in
  let id = Url.Current.get_fragment () in
  let entry =
    try
      json
      |> database_warning_entries_of_json
      |> List.find (fun entry -> string_of_int entry.id = id)
    with
    | Not_found -> failwith "invalid id"
  in
  set_div_ocaml_code_view entry.warning_result;
  Js._true

let () =
  Dom_html.window##onload <- Dom_html.handler onload

					      
