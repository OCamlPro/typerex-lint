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
  let (str_js : Js.js_string Js.t) = Js.Unsafe.variable "json" (* lint_web.var_json_name *) in
  let json = Yojson.Basic.from_string (Js.to_string str_js) in
  let raw_entries = tmp_json_db_raw_entries json in
  let entries = group_by (fun entry -> entry.f_plugin_name) raw_entries in
    
  let div = Dom_html.createDiv doc in
  List.iter begin fun (plugin_name,entries) ->
						  
    let msg =
      plugin_name ^ " : " ^ (string_of_int (List.length entries)) ^ " warning(s)"
    in
    let entry_line = Dom_html.createDiv doc in
    entry_line##innerHTML <- Js.string msg;
    let warnings_ul = Dom_html.createUl doc in
    
    List.iter begin fun entry ->
      let warning_li = Dom_html.createLi doc in
      let warning_div = Dom_html.createDiv doc in
      Dom.appendChild warning_div (warning_location_node entry.f_warning.loc);
      Dom.appendChild warning_div (warning_output_node entry.f_warning.output);
      Dom.appendChild warning_li warning_div;
      Dom.appendChild warnings_ul warning_li;
      Dom.appendChild warnings_ul (Dom_html.createBr doc)
    end entries;
    
    Dom.appendChild div entry_line;
    Dom.appendChild div warnings_ul;
    Dom.appendChild div (Dom_html.createBr doc)
  end entries;
  Dom.appendChild doc##body div;
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload
			      
