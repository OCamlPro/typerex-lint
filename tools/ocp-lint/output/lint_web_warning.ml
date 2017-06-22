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
  id : int;
  
  file_name : string;
  hash : Digest.t;
  lines_count : int;
  
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
   
let json_of_warning warn =
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

let json_of_digest digest =
  `String (Digest.to_hex digest)

let digest_of_json json =
  json |> to_string |> Digest.from_hex    

let json_of_database_warning_entry entry =
  `Assoc [
     ("id", `Int entry.id);
     ("file_name", `String entry.file_name);
     ("hash", json_of_digest entry.hash);
     ("lines_count", `Int entry.lines_count);
     ("plugin_name", `String entry.plugin_name);
     ("linter_name", `String entry.linter_name);
     ("linter_version", `String entry.linter_version);
     ("warning_result", json_of_warning entry.warning_result)
   ]

let database_warning_entry_of_json json  =
  let id = json |> member "id" |> to_int in
  let file_name = json |> member "file_name" |> to_string in
  let hash = json |> member "hash" |> digest_of_json in
  let lines_count = json |> member "lines_count" |> to_int in
  let plugin_name = json |> member "plugin_name" |> to_string in
  let linter_name = json |> member "linter_name" |> to_string in
  let linter_version = json |> member "linter_version" |> to_string in
  let warning_result = json |> member "warning_result" |> warning_of_json in
  {
    id = id;
    file_name = file_name;
    hash = hash;
    lines_count = lines_count;
    plugin_name = plugin_name;
    linter_name = linter_name;
    linter_version = linter_version;
    warning_result = warning_result
  }

let json_of_database_warning_entries entries =
  `List (List.map json_of_database_warning_entry entries)

let database_warning_entries_of_json json  =
  json |> to_list |> List.map database_warning_entry_of_json
