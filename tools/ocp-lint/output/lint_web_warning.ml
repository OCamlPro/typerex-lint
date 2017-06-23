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
  warning_id : int;

  warning_file_name : string;
  warning_hash : Digest.t;
  warning_file_lines_count : int;

  warning_plugin_name : string;
  warning_linter_name : string;
  warning_linter_version : string;
  (* option / source *)
  warning_result : Lint_warning_types.warning;
}

let warning_entries_group_by clss lst = (*** todo changer implantation ***)
  let rec aux acc = function
    | [] -> acc
    | (cx, x) :: y -> (*** todo changer ***)
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
     ("warning_id", `Int entry.warning_id);
     ("warning_file_name", `String entry.warning_file_name);
     ("warning_hash", json_of_digest entry.warning_hash);
     ("warning_file_lines_count", `Int entry.warning_file_lines_count);
     ("warning_plugin_name", `String entry.warning_plugin_name);
     ("warning_linter_name", `String entry.warning_linter_name);
     ("warning_linter_version", `String entry.warning_linter_version);
     ("warning_result", json_of_warning entry.warning_result)
   ]

let database_warning_entry_of_json json  =
  let warning_id =
    json |> member "warning_id" |> to_int
  in
  let warning_file_name =
    json |> member "warning_file_name" |> to_string
  in
  let warning_hash =
    json |> member "warning_hash" |> digest_of_json
  in
  let warning_file_lines_count =
    json |> member "warning_file_lines_count" |> to_int
  in
  let warning_plugin_name =
    json |> member "warning_plugin_name" |> to_string
  in
  let warning_linter_name =
    json |> member "warning_linter_name" |> to_string
  in
  let warning_linter_version =
    json |> member "warning_linter_version" |> to_string
  in
  let warning_result =
    json |> member "warning_result" |> warning_of_json
  in
  {
    warning_id = warning_id;
    warning_file_name = warning_file_name;
    warning_hash = warning_hash;
    warning_file_lines_count = warning_file_lines_count;
    warning_plugin_name = warning_plugin_name;
    warning_linter_name = warning_linter_name;
    warning_linter_version = warning_linter_version;
    warning_result = warning_result
  }

let json_of_database_warning_entries entries =
  `List (List.map json_of_database_warning_entry entries)

let database_warning_entries_of_json json  =
  json |> to_list |> List.map database_warning_entry_of_json
