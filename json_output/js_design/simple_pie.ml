open Yojson.Basic.Util

let doc = Dom_html.document

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
	    
let piechart id values =
  let _ =
    C3.Pie.make
      ~sectors:values ()
    |> C3.Pie.render ~bindto:("#" ^ id) in
  ()

let load_main_page entries =
  let id_piechart = "piechart" in
  let div_piechart = Dom_html.createDiv doc in
  let plugins_warnings = group_by (fun entry -> entry.f_plugin_name) entries in
  let values =
    List.map begin fun (plugin_name, entries) ->
      (plugin_name, float_of_int (List.length entries))
    end plugins_warnings
  in
  (*******)
  div_piechart##setAttribute(Js.string "id", Js.string id_piechart);
  (*******)
  Dom.appendChild doc##body div_piechart;
  piechart id_piechart values;

  List.iter begin fun (plugin_name,_) ->
    let plugin_name =
      Regexp.global_replace (Regexp.regexp_string "_") plugin_name "-"
    in
    let class_name = "c3-target-" ^ plugin_name in
    let elements = doc##getElementsByClassName (Js.string class_name) in
    Firebug.console##log (Js.string class_name);
    Firebug.console##log (elements);
    let piechart_part =
      if elements##length = 1 then
	elements##item (0)
      else
	elements##item (0)
        (*failwith ("invalids elements with class '" ^ class_name ^ "'")*)
    in
    ignore piechart_part;
    Firebug.console##log ((*piechart_part*) Js.string "--")
   end values
    
let onload _ =
  let (str_js: Js.js_string Js.t) = Js.Unsafe.variable "json" (* lint_web.var_json_name *) in
  let json = Yojson.Basic.from_string (Js.to_string str_js) in
  let entries = tmp_json_db_raw_entries json in
  load_main_page entries;
  Js._true

let () =
  Dom_html.window##onload <- Dom_html.handler onload
