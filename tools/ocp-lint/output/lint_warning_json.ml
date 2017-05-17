(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open Yojson.Basic
open Yojson.Basic.Util
open Lint_warning_types
open Lint_db_types

type database_warning_entry = {
  file_name : string;
  hash : Digest.t;
  plugin_name : string;
  linter_name : string;
  linter_version : string;
  (* option / source *)
  warning_result : Lint_warning_types.warning;
}
           
let json_of_position pos =
  let open Lexing in
  `Assoc [
     ("pos_fname", `String pos.pos_fname);
     ("pos_lnum", `Int pos.pos_lnum);
     ("pos_bol", `Int pos.pos_bol);
     ("pos_cnum", `Int pos.pos_cnum)
   ]

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
   
let json_of_location loc =
  let open Location in
  `Assoc [
     ("loc_start", json_of_position loc.loc_start);
     ("loc_end", json_of_position loc.loc_end);
     ("loc_ghost", `Bool loc.loc_ghost)
   ]

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
						  
let json_of_warning_declaration decl =
  `Assoc [
     ("short_name", `String decl.short_name);
     ("message", `String decl.message);
     ("id", `Int decl.id);
     ("severity", `Int decl.severity)
   ]

let warning_declaration_of_json json =
  let short_name = json |> member "short_name" |>  to_string in
  let message = json |> member "message" |> to_string in
  let id = json |> member "id" |> to_int in
  let severity = json |> member "severity" |> to_int in
  {short_name = short_name; message = message; id = id; severity = severity}
   
let json_of_warning ~warn =
  `Assoc [
     ("loc", json_of_location warn.loc);
     ("decl", json_of_warning_declaration warn.decl);
     ("output", `String warn.output)
   ]

let warning_of_json json  =
  let loc = json |> member "loc" |> location_of_json in
  let decl = json |> member "decl" |> warning_declaration_of_json in
  let output = json |> member "output" |> to_string in
  {
    loc = loc;
    decl = decl;
    output = output
  }

let json_of_source = function
  | Cache -> `String "Cache"
  | Analyse -> `String "Analyse"

let source_of_json json =
  let str = to_string json in
  match str with
  | "Cache" -> Cache
  | "Analyse" -> Analyse
  | _ -> raise (Type_error ("'" ^ str ^ "' is not a valid source", json))
		
let json_of_option (k,v) =
  `Assoc [
     ("opt_key", `String k);
     ("opt_value", `String v)
   ]

let option_of_json json =
  let key = json |> member "opt_key" |> to_string in
  let value = json |> member "opt_value" |> to_string in
  (key,value)
   
let json_of_linter_result linter_res =
  `Assoc [
     ("res_version", `String linter_res.res_version);
     ("res_source", json_of_source linter_res.res_source);
     ("res_options", `List (List.map json_of_option linter_res.res_options));
     ("res_warnings", `List (List.map begin fun warn ->
         json_of_warning ~warn:warn
       end linter_res.res_warnings))
   ]

let linter_result_of_json json =
  let res_version =
    json |> member "res_version" |> to_string
  in
  let res_source =
    json |> member "res_source" |> source_of_json
  in
  let res_options =
    json |> member "res_options" |> to_list |> List.map option_of_json
  in
  let res_warnings =
    json |> member "res_warnings" |> to_list |> List.map warning_of_json
  in
  {
    res_version = res_version;
    res_source = res_source;
    res_options = res_options;
    res_warnings = res_warnings
  }

let json_of_linter_map linter_map =
  `List (StringMap.fold begin fun linter_name linter_result acc ->
    `Assoc [
       ("linter_name", `String linter_name);
       ("linter_result", json_of_linter_result linter_result)
    ] :: acc
  end linter_map [])

let linter_map_of_json json =
  json |> to_list |> List.fold_left begin fun acc json_entry ->
    let linter_name =
      json_entry |> member "linter_name" |> to_string in
    let linter_result =
      json_entry |> member "linter_result" |> linter_result_of_json
    in
    StringMap.add linter_name linter_result acc
  end StringMap.empty

let json_of_plugin_map plugin_map =
  `List (StringMap.fold begin fun plugin_name linter_map acc ->
    `Assoc [
       ("plugin_name", `String plugin_name);
       ("linter_map", json_of_linter_map linter_map)
    ] :: acc
  end plugin_map [])

let plugin_map_of_json json =
  json |> to_list |> List.fold_left begin fun acc json_entry ->
    let plugin_name =
      json_entry |> member "plugin_name" |> to_string in
    let linter_map =
      json_entry |> member "linter_map" |> linter_map_of_json
    in
    StringMap.add plugin_name linter_map acc
  end StringMap.empty

let json_of_digest digest =
  `String (Digest.to_hex digest)

let digest_of_json json =
  json |> to_string |> Digest.from_hex
				    
let json_of_file_map (digest, plugin_map) =
  `Assoc [
    ("digest", json_of_digest digest);
    ("plugin_map", json_of_plugin_map plugin_map) 
  ]

let file_map_of_json json =
  let digest =
    json |> member "digest" |> digest_of_json
  in
  let plugin_map =
    json |> member "plugin_map" |> plugin_map_of_json
  in
  (digest, plugin_map)

let json_of_db ~db =
  `List (Hashtbl.fold begin fun file_name file_map acc ->
    `Assoc [
       ("file_name", `String file_name);
       ("file_map", json_of_file_map file_map)
    ] :: acc
  end db [])

let unsafe_db_of_json ~json =
  let db = Hashtbl.create 1024 in
  json |> to_list |> List.iter begin fun json_entry ->
    let file_name =
      json_entry |> member "file_name" |> to_string
    in
    let file_map =
      json_entry |> member "file_map" |> file_map_of_json
    in
    Hashtbl.add db file_name file_map 
  end;
  db
    
let db_of_json ~json =
  try
    Some (unsafe_db_of_json json)
  with
  | Type_error _ | Undefined _ -> None 

let raw_entries ~db =
  Hashtbl.fold begin fun file_name (hash, plugin_map) acc ->
    StringMap.fold begin fun plugin_name linter_map acc ->
      StringMap.fold begin fun linter_name linter_result acc ->
        List.fold_left begin fun acc warning_result ->
	  {
	    file_name = file_name;
	    hash = hash;
	    plugin_name = plugin_name;
	    linter_name = linter_name;
	    linter_version = linter_result.res_version;
	    warning_result = warning_result;
	  } :: acc		       
	end acc linter_result.res_warnings	
      end linter_map acc					      
    end plugin_map acc 
  end db []
