let doc = Dom_html.document

(*********************** A SUPPRIMER ***************************************)

open Yojson.Basic.Util
	    
type tmp_warning = {
  output : string;
  loc : Location.t
}
	    
type tmp_db_entry = {
  file_name : string;
  digest : Digest.t;
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
	digest = json_entry |> member "file_map" |> member "digest" |> to_string |> Digest.from_hex;
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
	    
let onload _ =
  let (str_js: Js.js_string Js.t) = Js.Unsafe.variable "json" (* lint_web.var_json_name *) in
  let json = Yojson.Basic.from_string (Js.to_string str_js) in
  let db = tmp_db_entries_of_json json in
  let div = Dom_html.createDiv doc in
  List.iter begin fun entry ->
    let iframe = Dom_html.createIframe doc in
    iframe##setAttribute(Js.string "src", Js.string (Digest.to_hex entry.digest ^ ".html"));
    iframe##setAttribute(Js.string "width", Js.string "700");
    iframe##setAttribute(Js.string "height", Js.string "500");
    Dom.appendChild div iframe;
    Dom.appendChild div (Dom_html.createBr doc);
  end db;
  Dom.appendChild doc##body div;
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload
			      
