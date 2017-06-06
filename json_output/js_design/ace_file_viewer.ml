open Ace

(*******)
let find_component id =
  match Js_utils.Manip.by_id id with
  | Some div -> div
  | None -> failwith ("Cannot find id " ^ id)
(*******)

(*********************** A SUPPRIMER ***************************************)

open Yojson.Basic.Util

type tmp_warning = {
  output : string;
  loc : Location.t
}

type database_warning_entry = {
  file_name : string;
  hash : Digest.t;
  plugin_name : string;
  linter_name : string;
  linter_version : string;
  (* option / source *)
  warning_result : tmp_warning;
}

let position_of_json json =
  let open Lexing in
  let pos_fname = json |> member "pos_fname" |> to_string in
  let pos_lnum = json |> member "pos_lnum" |> to_int in
  let pos_bol = json |> member "pos_bol" |> to_int in
  let pos_cnum = json |> member "pos_cnum" |> to_int in
  {
    pos_fname = pos_fname;
    pos_lnum = pos_lnum;
    pos_bol = pos_bol;
    pos_cnum = pos_cnum
  }

let location_of_json json =
  let open Location in
  let loc_start = json |> member "loc_start" |> position_of_json in
  let loc_end = json |> member "loc_end" |> position_of_json in
  let loc_ghost = json |> member "loc_ghost" |> to_bool in
  {
    loc_start = loc_start;
    loc_end = loc_end;
    loc_ghost = loc_ghost
  }
		      
let tmp_warning_of_json json =
  {
    output = json |> member "output" |> to_string;
    loc = json |> member "loc" |> location_of_json  
  }

let digest_of_json json =
  json |> to_string |> Digest.from_hex    

let database_warning_entry_of_json json  =
  let file_name = json |> member "file_name" |> to_string in
  let hash = json |> member "hash" |> digest_of_json in
  let plugin_name = json |> member "plugin_name" |> to_string in
  let linter_name = json |> member "linter_name" |> to_string in
  let linter_version = json |> member "linter_version" |> to_string in
  let warning_result = json |> member "warning_result" |> tmp_warning_of_json in
  {
    file_name = file_name;
    hash = hash;
    plugin_name = plugin_name;
    linter_name = linter_name;
    linter_version = linter_version;
    warning_result = warning_result
  }

let database_warning_entries_of_json json  =
  json |> to_list |> List.map database_warning_entry_of_json

			      
let group_by clss lst =
  let rec aux acc = function
    | [] -> acc
    | (cx, x) :: y -> (*** ptt changer ***)
       begin match acc with
       | (cx', x') :: y' when cx = cx' ->
          aux ((cx, x :: x') :: y') y
       | _ ->
	  aux ((cx, [x]) :: acc) y
       end
  in
  lst
  |> List.map (fun x -> clss x, x) (*** ***)
  |> List.sort (fun (c,_) (c',_) -> Pervasives.compare c c')
  |> aux []
		
(************************************************************************)
		     
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
  (Array.fold_left begin fun acc line ->
    acc ^ join ^ line 
  end hd tl)
    
let ace_loc_of_warning_loc warning_loc context =
  let aux pos =
    pos.Lexing.pos_lnum,
    pos.Lexing.pos_cnum - pos.Lexing.pos_bol
  in
  let bl,bc = aux warning_loc.Location.loc_start in
  let el,ec = aux warning_loc.Location.loc_end in
  {
    loc_start = context + 1, bc;
    loc_end = context + el - bl + 1, ec;
  }

let code_viewer_register_warnings ace warning =
  let blcntxt =
    warning.loc.Location.loc_start.Lexing.pos_lnum - context_line_number
  in
  let elcntxt =
    warning.loc.Location.loc_end.Lexing.pos_lnum + context_line_number
  in
  let loc = ace_loc_of_warning_loc warning.loc context_line_number in
  let content =
    array_joining "\n" (Ace.get_lines ace (blcntxt-1) (elcntxt-1))
  in
  Firebug.console##log (Js.string content);
  Firebug.console##log (blcntxt);
  Firebug.console##log (elcntxt);
  Ace.set_value ace content;
  Ace.clear_selection ace;
  Ace.set_option ace "firstLineNumber" (blcntxt);
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
  let warnings =
    List.map (fun entry -> entry.warning_result) (database_warning_entries_of_json json)
  in
  (*** ***)
  let warning = List.hd warnings in
  (*** ***)
  set_div_ocaml_code_view warning;
  Js._true

let () =
  Dom_html.window##onload <- Dom_html.handler onload

					      
