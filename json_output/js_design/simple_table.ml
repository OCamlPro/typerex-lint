let doc = Dom_html.document

open Yojson.Basic.Util
	    
(* tmp* a supprimer *)

(*********************** A SUPPRIMER ***************************************)
	    
type tmp_warning = {
  output : string;
  loc : Location.t
}

type tmp_db_flat_entry = {
  f_file_name : string;
  f_plugin_name : string;
  f_linter_name : string;
  f_hash : Digest.t;
  f_warning : tmp_warning
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
    
let tmp_json_db_raw_entries json =
  json
  |> to_list
  |> List.fold_left begin fun acc json_file_map ->
       let file_name =
	 json_file_map |> member "file_name" |> to_string
       in
       let file_map =
	 json_file_map |>  member "file_map"
       in
       let hash =
	 file_map |> member "digest" |> to_string |> Digest.from_hex
       in
       file_map
       |> member "plugin_map"
       |> to_list
       |> List.fold_left begin fun acc json_plugin ->
	  let plugin_name =
	    json_plugin |> member "plugin_name" |> to_string
	  in
	  json_plugin
	  |> member "linter_map"
	  |> to_list
	  |> List.fold_left begin fun acc json_linter ->
	     let linter_name =
	       json_linter |> member "linter_name" |> to_string
	     in
	     let linter_result =
	       json_linter |> member "linter_result"
	     in
	     linter_result
	     |> member "res_warnings"
	     |> to_list
	     |> List.fold_left begin fun acc json_warning ->
		  let warning = tmp_warning_of_json json_warning in
		  {
		    f_file_name = file_name;
		    f_plugin_name = plugin_name;
		    f_linter_name = linter_name;
		    f_hash = hash;
		    f_warning = warning;	       
		  } :: acc
	        end acc
	     end acc
          end acc 
     end []

let id_of_loc loc =
  (string_of_int loc.Location.loc_start.Lexing.pos_lnum)
  ^ "-" ^
  (string_of_int loc.Location.loc_end.Lexing.pos_lnum)
		
(************************************************************************)

let summary_entry_file_name_col entry =
  let p = Dom_html.createP doc in
  let a = Dom_html.createA doc in
  let href = (Digest.to_hex entry.f_hash) ^ ".html#code." ^ (id_of_loc entry.f_warning.loc)  in
  a##innerHTML <- Js.string entry.f_file_name;
  if not entry.f_warning.loc.Location.loc_ghost then begin
    a##setAttribute(Js.string "href", Js.string href);
    a##setAttribute(Js.string "target", Js.string "_blank");
  end;
  Dom.appendChild p a;
  p

let summary_entry_plugin_name_col entry =
  let p = Dom_html.createP doc in
  p##innerHTML <- Js.string entry.f_plugin_name;
  p

let summary_entry_linter_name_col entry =
  let p = Dom_html.createP doc in
  p##innerHTML <- Js.string entry.f_linter_name;
  p

let summary_entry_warning_output_col entry =
  let p = Dom_html.createP doc in
  let a = Dom_html.createA doc in
  let href = "DOC.html" in
  a##innerHTML <- Js.string entry.f_warning.output;
  a##setAttribute(Js.string "href", Js.string href);
  a##setAttribute(Js.string "target", Js.string "_blank");
  Dom.appendChild p a;
  p

let summary_warning_entry entry =
  let tr = Dom_html.createTr doc in
  let td_file_name = Dom_html.createTd doc in
  let td_plugin_name = Dom_html.createTd doc in
  let td_linter_name = Dom_html.createTd doc in
  let td_warning_output = Dom_html.createTd doc in
  Dom.appendChild td_file_name (summary_entry_file_name_col entry);
  Dom.appendChild tr td_file_name;
  Dom.appendChild td_plugin_name (summary_entry_plugin_name_col entry);
  Dom.appendChild tr td_plugin_name;
  Dom.appendChild td_linter_name (summary_entry_linter_name_col entry);
  Dom.appendChild tr td_linter_name;
  Dom.appendChild td_warning_output (summary_entry_warning_output_col entry);
  Dom.appendChild tr td_warning_output;
  tr

let summary_head_description =
  let thead = Dom_html.createThead doc in
  let tr = Dom_html.createTr doc in
  let th_file_name = Dom_html.createTh doc in
  let th_plugin_name = Dom_html.createTh doc in
  let th_linter_name = Dom_html.createTh doc in
  let th_warning_output = Dom_html.createTh doc in
  th_file_name##innerHTML <- Js.string "File";
  Dom.appendChild tr th_file_name;
  th_plugin_name##innerHTML <- Js.string "Plugin";
  Dom.appendChild tr th_plugin_name;
  th_linter_name##innerHTML <- Js.string "Linter";
  Dom.appendChild tr th_linter_name;
  th_warning_output##innerHTML <- Js.string "Warning";
  Dom.appendChild tr th_warning_output;
  Dom.appendChild thead tr;
  thead
  
let summary_table entries =
  let table = Dom_html.createTable doc in
  let tbody = Dom_html.createTbody doc in
  (*******)
  table##setAttribute(Js.string "id", Js.string "summary-table");
  table##setAttribute(Js.string "class", Js.string "display");
  table##setAttribute(Js.string "cellspacing", Js.string "0");
  table##setAttribute(Js.string "width", Js.string "100%");
  (*******)
  Dom.appendChild table (summary_head_description);
  List.iter begin fun entry ->
    Dom.appendChild tbody (summary_warning_entry entry);
  end entries;
  Dom.appendChild table tbody;
  table

    (****
let create_summary_page db =
  let div = Dom_html.createDiv doc in
  div***)
    
let onload _ =
  let (str_js: Js.js_string Js.t) = Js.Unsafe.variable "json" (* lint_web.var_json_name *) in
  let json = Yojson.Basic.from_string (Js.to_string str_js) in
  let entries = tmp_json_db_raw_entries json in
  let div = Dom_html.createDiv doc in
  Dom.appendChild div (summary_table entries);
  Dom.appendChild doc##body div;
  ignore (
      (******* tool bar ici *******)
      Js.Unsafe.eval_string
  	"$(document).ready(function() {$('#summary-table').DataTable({paging: false, info:false}); $(\"div.toolbar\").html('<b>FORMULAIRE</b>')});");
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload			      
