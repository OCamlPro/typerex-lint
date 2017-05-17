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
  hash : Digest.t;
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
	hash = json_entry |> member "file_map"
	                  |> member "digest" |> to_string |> Digest.from_hex;
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

let id_of_loc loc =
  (string_of_int loc.Location.loc_start.Lexing.pos_lnum)
  ^ "-" ^
  (string_of_int loc.Location.loc_end.Lexing.pos_lnum)
		
(************************************************************************)
    
let warning_node fname output loc =
  let href = fname ^ "#code." ^ (id_of_loc loc)  in
  let node = Dom_html.createA doc in
  node##innerHTML <- Js.string output;
  if not loc.Location.loc_ghost then begin
    node##setAttribute(Js.string "href", Js.string href);
    node##setAttribute(Js.string "target", Js.string "_blank");
  end;
  node
    
let onload _ =
  let (str_js: Js.js_string Js.t) = Js.Unsafe.variable "json" (* lint_web.var_json_name *) in
  let json = Yojson.Basic.from_string (Js.to_string str_js) in
  let db = tmp_db_entries_of_json json in
  let div = Dom_html.createDiv doc in
  List.iter begin fun entry ->
    let nb_warnings = List.length entry.warnings in		  
    if nb_warnings > 0 then begin
      let fname = Digest.to_hex entry.hash ^ ".html" in
      let node_file_name = Dom_html.createA doc in
      let node_nb_warn = Dom_html.createP doc in
      node_file_name##innerHTML <- Js.string entry.file_name;
      node_file_name##setAttribute(Js.string "href", Js.string fname);
      node_file_name##setAttribute(Js.string "target", Js.string "_blank");
      node_nb_warn##innerHTML <- Js.string (string_of_int (List.length entry.warnings) ^ " warning(s) : ");
      let warnings_ul = Dom_html.createUl doc in
      List.iter begin fun warning ->
        let warning_li = Dom_html.createLi doc in
        let warning_div = Dom_html.createDiv doc in
        Dom.appendChild warning_div (warning_node fname warning.output warning.loc);
        Dom.appendChild warning_li warning_div;
        Dom.appendChild warnings_ul warning_li;
        Dom.appendChild warnings_ul (Dom_html.createBr doc)
      end entry.warnings;
      Dom.appendChild div node_file_name;
      Dom.appendChild div node_nb_warn;
      Dom.appendChild div warnings_ul;
      Dom.appendChild div (Dom_html.createBr doc)
    end
  end db;
  Dom.appendChild doc##body div;
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload
			      
