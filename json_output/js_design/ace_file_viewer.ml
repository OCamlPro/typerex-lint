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
		     
let doc = Dom_html.document

let create_ocaml_code_viewer div warnings =
  let loc = {
    loc_start = (0,0);
    loc_end = (3,3);
  } in
  
  let ace = Ace.create_editor div in
  Ace.set_mode ace "ace/mode/ocaml";
  Ace.set_tab_size ace 80;
  (* let editor = { ace; current_error = None; current_warnings = [] } in *)
  (* Ace.set_custom_data editor.ace editor; *)
  (* Ace.record_event_handler editor.ace "change" *)
  (*   (fun () -> Lwt.async (fun () -> reset_error editor)); *)
  (* Ace.add_keybinding editor.ace "backspace" "Shift-Backspace|Backspace" *)
  (*   do_delete; *)
  (* Ace.add_keybinding editor.ace "indent" "Tab" do_indent; *)
  Ace.set_font_size ace 14;
  Ace.set_read_only ace true;
  Ace.set_mark ace ~loc:loc ~type_:Ace.Warning "test"

let set_div_ocaml_code_view warnings =
  let code_div = find_component "code" (***** code id dans lint_web ****) in
  create_ocaml_code_viewer (Tyxml_js.To_dom.of_div code_div) warnings
    
let onload _ =
  let (str_js: Js.js_string Js.t) =
    Js.Unsafe.variable "json" (* lint_web.var_json_name *)
  in
  let json =
    Yojson.Basic.from_string (Js.to_string str_js)
  in
  let warnings =
    json
    |> tmp_json_db_raw_entries
    |> List.filter (fun entry -> true)
    |> List.map (fun entry -> entry.f_warning)
  in
  
  set_div_ocaml_code_view warnings;
  Js._true

let () =
  Dom_html.window##onload <- Dom_html.handler onload

					      
