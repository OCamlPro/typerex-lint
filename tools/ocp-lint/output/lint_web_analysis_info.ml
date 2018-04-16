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
open Unix

type file_info = {
  file_name : string;
  file_hash : Digest.t;
  file_lines_count : int;
}

type plugin_info = {
  plugin_name : string;
  plugin_description : string;
}

type linter_info = {
  linter_plugin : plugin_info;
  linter_name : string;
  linter_description : string;
}

type warning_info = {
  warning_id : int;
  warning_file : file_info;
  warning_linter : linter_info;
  warning_type : Lint_warning_types.warning;
}

type error_info = {
  error_id : int;
  error_file : file_info;
  error_type : Lint_db_types.error;
}

type analysis_info = {
  files_info : file_info list;
  plugins_info : plugin_info list;
  linters_info : linter_info list;
  warnings_info : warning_info list;
  errors_info : error_info list;
  analysis_root : string;
  analysis_date : Unix.tm;
}

let json_of_digest digest =
  `String (Digest.to_hex digest)

let digest_of_json json =
  json |> to_string |> Digest.from_hex

let json_of_file_info file_info =
  `Assoc [
     ("file_name",
      `String file_info.file_name);
     ("file_hash",
      json_of_digest file_info.file_hash);
     ("file_lines_count",
      `Int file_info.file_lines_count);
   ]

let file_info_of_json json  =
  let file_name =
    json
    |> member "file_name"
    |> to_string
  in
  let file_hash =
    json
    |> member "file_hash"
    |> digest_of_json
  in
  let file_lines_count =
    json
    |> member "file_lines_count"
    |> to_int
  in
  {
    file_name = file_name;
    file_hash = file_hash;
    file_lines_count = file_lines_count;
  }

let json_of_files_info files_info =
  `List (List.map json_of_file_info files_info)

let files_info_of_json json =
  json
  |> to_list
  |> List.map file_info_of_json

let json_of_plugin_info plugin_info =
  `Assoc [
     ("plugin_name",
      `String plugin_info.plugin_name);
     ("plugin_description",
      `String plugin_info.plugin_description);
   ]

let plugin_info_of_json json  =
  let plugin_name =
    json
    |> member "plugin_name"
    |> to_string
  in
  let plugin_description =
    json
    |> member "plugin_description"
    |> to_string
  in
  {
    plugin_name = plugin_name;
    plugin_description = plugin_description;
  }

let json_of_plugins_info plugins_info =
  `List (List.map json_of_plugin_info plugins_info)

let plugins_info_of_json json =
  json
  |> to_list
  |> List.map plugin_info_of_json

let json_of_linter_info linter_info =
  `Assoc [
     ("linter_plugin",
      json_of_plugin_info linter_info.linter_plugin);
     ("linter_name",
      `String linter_info.linter_name);
     ("linter_description",
      `String linter_info.linter_description);
   ]

let linter_info_of_json json  =
  let linter_plugin =
    json
    |> member "linter_plugin"
    |> plugin_info_of_json
  in
  let linter_name =
    json
    |> member "linter_name"
    |> to_string
  in
  let linter_description =
    json
    |> member "linter_description"
    |> to_string
  in
  {
    linter_plugin = linter_plugin;
    linter_name = linter_name;
    linter_description = linter_description;
  }

let json_of_linters_info linters_info =
  `List (List.map json_of_linter_info linters_info)

let linters_info_of_json json =
  json
  |> to_list
  |> List.map linter_info_of_json

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
  {
    short_name = short_name;
    message = message;
    id = id;
    severity = severity;
  }

let json_of_warning_type warning_type =
  `Assoc [
     ("loc", json_of_location warning_type.loc);
     ("decl", json_of_warning_declaration warning_type.decl);
     ("output", `String warning_type.output)
   ]

let warning_type_of_json json  =
  let loc = json |> member "loc" |> location_of_json in
  let decl = json |> member "decl" |> warning_declaration_of_json in
  let output = json |> member "output" |> to_string in
  {
    loc = loc;
    decl = decl;
    output = output
  }

let json_of_warning_info warning_info =
  `Assoc [
     ("warning_id",
      `Int warning_info.warning_id);
     ("warning_file",
      json_of_file_info warning_info.warning_file);
     ("warning_linter",
      json_of_linter_info warning_info.warning_linter);
     ("warning_type",
      json_of_warning_type warning_info.warning_type);
   ]

let warning_info_of_json json  =
  let warning_id =
    json
    |> member "warning_id"
    |> to_int
  in
  let warning_file =
    json
    |> member "warning_file"
    |> file_info_of_json
  in
  let warning_linter =
    json
    |> member "warning_linter"
    |> linter_info_of_json
  in
  let warning_type =
    json
    |> member "warning_type"
    |> warning_type_of_json
  in
  {
    warning_id = warning_id;
    warning_file = warning_file;
    warning_linter = warning_linter;
    warning_type = warning_type;
  }

let json_of_warnings_info warnings_info =
  `List (List.map json_of_warning_info warnings_info)

let warnings_info_of_json json =
  json
  |> to_list
  |> List.map warning_info_of_json

let json_of_db_error db_error =
  let open Lint_db_error in
  match db_error with
  | File_not_found filename ->
     `Assoc [
        ("File_not_found",
         `List [`String filename]);
      ]
  | File_not_in_db filename ->
     `Assoc [
        ("File_not_in_db",
         `List [`String filename]);
      ]
  | Plugin_not_in_db (fname, pname) ->
     `Assoc [
        ("Plugin_not_in_db",
         `List [`String fname; `String pname]);
      ]
  | Linter_not_in_db (fname, pname, lname) ->
     `Assoc [
        ("Linter_not_in_db",
         `List [`String fname; `String pname; `String lname]);
      ]
  | No_db_found ->
     `Assoc [
        ("No_db_found",
         `List []);
      ]

let db_error_of_json json =
  match to_assoc json with
  | [err_type, err_args] ->
     begin match err_type, to_list err_args with
     | ("File_not_found", [filename]) ->
        Lint_db_error.File_not_found
          (to_string filename)
     | ("File_not_in_db", [filename]) ->
        Lint_db_error.File_not_in_db
          (to_string filename)
     | ("Plugin_not_in_db", [fname; pname]) ->
        Lint_db_error.Plugin_not_in_db
          (to_string fname, to_string pname)
     | ("Linter_not_in_db", [fname; pname; lname]) ->
        Lint_db_error.Linter_not_in_db
          (to_string fname, to_string pname, to_string lname)
     | ("No_db_found", []) ->
        Lint_db_error.No_db_found
     | _ ->
        raise (Type_error ("object is not a valid error", json))
     end
  | _ ->
     raise (Type_error ("object is not a valid error", json))

let json_of_plugin_module plugin_module =
  let module P = (val plugin_module : Lint_plugin_types.PLUGIN) in
  `Assoc [
     ("name", `String P.name);
     ("short_name", `String P.short_name);
     ("details", `String P.details);
     ("enabled", `Bool P.enabled);
   ]

let plugin_module_of_json json =
  (module struct
    let name = json |> member "name" |> to_string
    let short_name = json |> member "short_name" |> to_string
    let details = json |> member "details" |> to_string
    let enabled = json |> member "enabled" |> to_bool
   end : Lint_plugin_types.PLUGIN)

let json_of_exception exn =
  `String (Printexc.to_string exn)

let exception_of_json json =
  Failure (to_string json)

let json_of_plugin_error plugin_error =
  let open Lint_plugin_error in
  match plugin_error with
  | Plugin_already_registered plugin ->
     `Assoc [
        ("Plugin_already_registered",
         `List [json_of_plugin_module plugin]);
      ]
  | Linter_already_registered lname ->
     `Assoc [
        ("Linter_already_registered",
         `List [`String lname]);
      ]
  | Plugin_not_found plugin ->
     `Assoc [
        ("Plugin_not_found",
         `List [json_of_plugin_module plugin]);
      ]
  | Patch_file_not_found filename ->
     `Assoc [
        ("Patch_file_not_found",
         `List [`String filename]);
      ]
  | Sempatch_failure str ->
     `Assoc [
        ("Sempatch_failure",
         `List [`String str]);
      ]
  | Syntax_error filename ->
     `Assoc [
        ("Syntax_error",
         `List [`String filename]);
      ]
  | Plugin_error s ->
     `Assoc [
        ("Plugin_error",
         `List [`String s]);
      ]

let plugin_error_of_json json =
  match to_assoc json with
  | [err_type, err_args] ->
     begin match err_type, to_list err_args with
     | ("Plugin_already_registered", [plugin]) ->
        Lint_plugin_error.Plugin_already_registered
          (plugin_module_of_json plugin)
     | ("Linter_already_registered", [lname]) ->
        Lint_plugin_error.Linter_already_registered
          (to_string lname)
     | ("Plugin_not_found", [plugin]) ->
        Lint_plugin_error.Plugin_not_found
          (plugin_module_of_json plugin)
     | ("Patch_file_not_found", [filename]) ->
        Lint_plugin_error.Patch_file_not_found
          (to_string filename)
     | ("Sempatch_failure", [str]) ->
        Lint_plugin_error.Sempatch_failure
          (to_string str)
     | ("Syntax_error", [filename]) ->
        Lint_plugin_error.Syntax_error
          (to_string filename)
     | ("Plugin_error", [`String s]) ->
        Lint_plugin_error.Plugin_error s
     | _ ->
        raise (Type_error ("object is not a valid error", json))
     end
  | _ ->
     raise (Type_error ("object is not a valid error", json))

let json_of_error = function
  | Db_error db_error ->
     `Assoc [
        ("Db_error",
         `List [json_of_db_error db_error]);
      ]
  | Plugin_error plugin_error ->
     `Assoc [
        ("Plugin_error",
         `List [json_of_plugin_error plugin_error]);
      ]
  | Sempatch_error str ->
     `Assoc [
        ("Sempatch_error",
         `List [`String str]);
      ]
  | Ocplint_error str ->
     `Assoc [
        ("Ocplint_error",
         `List [`String str]);
      ]

let error_of_json json =
  match to_assoc json with
  | [err_type, err_args] ->
     begin match err_type, to_list err_args with
     | ("Db_error", [db_error]) ->
        Db_error (db_error_of_json db_error)
     | ("Plugin_error", [plugin_error]) ->
        Plugin_error (plugin_error_of_json plugin_error)
     | ("Sempatch_error", [str]) ->
        Sempatch_error (to_string str)
     | ("Ocplint_error", [str]) ->
        Ocplint_error (to_string str)
     | _ ->
        raise (Type_error ("object is not a valid error", json))
     end
  | _ ->
     raise (Type_error ("object is not a valid error", json))

let json_of_error_info error_info =
  `Assoc [
     ("error_id",
      `Int error_info.error_id);
     ("error_file",
      json_of_file_info error_info.error_file);
     ("error_type",
      json_of_error error_info.error_type);
   ]

let error_info_of_json json  =
  let error_id =
    json
    |> member "error_id"
    |> to_int
  in
  let error_file  =
    json
    |> member "error_file"
    |> file_info_of_json
  in
  let error_type =
    json
    |> member "error_type"
    |> error_of_json
  in
  {
    error_id = error_id;
    error_file = error_file;
    error_type = error_type;
  }

let json_of_errors_info errors_info =
  `List (List.map json_of_error_info errors_info)

let errors_info_of_json json =
  json
  |> to_list
  |> List.map error_info_of_json

let json_of_unix_time unix_time =
  `Assoc [
     ("tm_sec",
      `Int unix_time.tm_sec);
     ("tm_min",
      `Int unix_time.tm_min);
     ("tm_hour",
      `Int unix_time.tm_hour);
     ("tm_mday",
      `Int unix_time.tm_mday);
     ("tm_mon",
      `Int unix_time.tm_mon);
     ("tm_year",
      `Int unix_time.tm_year);
     ("tm_wday",
      `Int unix_time.tm_wday);
     ("tm_yday",
      `Int unix_time.tm_yday);
     ("tm_isdst",
      `Bool unix_time.tm_isdst);
   ]

let unix_time_of_json json =
  let tm_sec =
    json
    |> member "tm_sec"
    |> to_int
  in
  let tm_min =
    json
    |> member "tm_min"
    |> to_int
  in
  let tm_hour =
    json
    |> member "tm_hour"
    |> to_int
  in
  let tm_mday =
    json
    |> member "tm_mday"
    |> to_int
  in
  let tm_mon =
    json
    |> member "tm_mon"
    |> to_int
  in
  let tm_year =
    json
    |> member "tm_year"
    |> to_int
  in
  let tm_wday =
    json
    |> member "tm_wday"
    |> to_int
  in
  let tm_yday =
    json
    |> member "tm_yday"
    |> to_int
  in
  let tm_isdst =
    json
    |> member "tm_isdst"
    |> to_bool
  in
  {
    tm_sec = tm_sec;
    tm_min = tm_min;
    tm_hour = tm_hour;
    tm_mday = tm_mday;
    tm_mon = tm_mon;
    tm_year = tm_year;
    tm_wday = tm_wday;
    tm_yday = tm_yday;
    tm_isdst = tm_isdst;
  }

let json_of_analysis_info analysis_info =
  `Assoc [
     ("files_info",
      json_of_files_info analysis_info.files_info);
     ("plugins_info",
      json_of_plugins_info analysis_info.plugins_info);
     ("linters_info",
      json_of_linters_info analysis_info.linters_info);
     ("warnings_info",
      json_of_warnings_info analysis_info.warnings_info);
     ("errors_info",
      json_of_errors_info analysis_info.errors_info);
     ("analysis_root",
      `String analysis_info.analysis_root);
     ("analysis_date",
      json_of_unix_time analysis_info.analysis_date);
   ]

let analysis_info_of_json json  =
  let files_info =
    json
    |> member "files_info"
    |> to_list
    |> List.map file_info_of_json
  in
  let plugins_info =
    json
    |> member "plugins_info"
    |> to_list
    |> List.map plugin_info_of_json
  in
  let linters_info =
    json
    |> member "linters_info"
    |> linters_info_of_json
  in
  let warnings_info =
    json
    |> member "warnings_info"
    |> warnings_info_of_json
  in
  let errors_info =
    json
    |> member "errors_info"
    |> errors_info_of_json
  in
  let analysis_root =
    json
    |> member "analysis_root"
    |> to_string
  in
  let analysis_date =
    json
    |> member "analysis_date"
    |> unix_time_of_json
  in
  {
    files_info = files_info;
    plugins_info = plugins_info;
    linters_info = linters_info;
    warnings_info = warnings_info;
    errors_info = errors_info;
    analysis_root = analysis_root;
    analysis_date = analysis_date;
  }
