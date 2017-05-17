let doc = Dom_html.document

open Yojson.Basic.Util
	    
(* tmp* a supprimer *)

(*********************** A SUPPRIMER ***************************************)
	    
type tmp_warning = {
  output : string;
  loc : Location.t
}
	    
type tmp_db_entry = {
  file_name : string;
  warnings : tmp_warning list
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
    
let tmp_db_entries_of_json json =
  json
    |> to_list
    |> List.map begin fun json_entry ->
      {
        file_name = json_entry |> member "file_name" |> to_string;
        warnings = json_entry
          |> member "file_map"
          |> member "plugin_map"
          |> to_list
          |> filter_member "linter_map"
          |> flatten
          |> filter_member "linter_result"
          |> filter_member "res_warnings"
          |> flatten
          |> List.map tmp_warning_of_json
      } end
		
(************************************************************************)

let warning_location_node loc =
  let div = Dom_html.createDiv doc in
  let fname_div = Dom_html.createDiv doc in
  let fpos_div = Dom_html.createDiv doc in
  fname_div##innerHTML <- Js.string loc.Location.loc_start.Lexing.pos_fname;
  let str_line =
    Printf.sprintf "Lines [%d - %d]"
      loc.Location.loc_start.Lexing.pos_lnum
      loc.Location.loc_end.Lexing.pos_lnum
  in
  fpos_div##innerHTML <- Js.string str_line;
  Dom.appendChild div fname_div;
  Dom.appendChild div fpos_div;
  div

let warning_output_node output =
  let div = Dom_html.createDiv doc in
  div##innerHTML <- Js.string output;
  div
    
let onload _ =
  let (str_js: Js.js_string Js.t) = Js.Unsafe.variable "json" (* lint_web.var_json_name *) in
  let json = Yojson.Basic.from_string (Js.to_string str_js) in
  let db = tmp_db_entries_of_json json in
  let div = Dom_html.createDiv doc in
  List.iter begin fun entry ->
    let entry_line = Dom_html.createDiv doc in
    entry_line##innerHTML <- Js.string (entry.file_name ^ " : " ^ string_of_int (List.length entry.warnings) ^ " warning(s)");
    let warnings_ul = Dom_html.createUl doc in
    List.iter begin fun warning ->
      let warning_li = Dom_html.createLi doc in
      let warning_div = Dom_html.createDiv doc in
      Dom.appendChild warning_div (warning_location_node warning.loc);
      Dom.appendChild warning_div (warning_output_node warning.output);
      Dom.appendChild warning_li warning_div;
      Dom.appendChild warnings_ul warning_li;
      Dom.appendChild warnings_ul (Dom_html.createBr doc)
    end entry.warnings;
    Dom.appendChild div entry_line;
    Dom.appendChild div warnings_ul;
    Dom.appendChild div (Dom_html.createBr doc)
  end db;
  Dom.appendChild doc##body div;
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload
			      
