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

type plugins_database_entry = {
  plugin_name : string;
  plugin_description : string;
  plugin_linter_name : string;
  plugin_linter_description : string;
}

let json_of_plugins_database_entry entry =
  `Assoc [
     ("plugin_name",
      `String entry.plugin_name);
     ("plugin_description",
      `String entry.plugin_description);
     ("plugin_linter_name",
      `String entry.plugin_linter_name);
     ("plugin_linter_description",
      `String entry.plugin_linter_description)
   ]

let plugins_database_entry_of_json json  =
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
  let plugin_linter_name =
    json
    |> member "plugin_linter_name"
    |> to_string
  in
  let plugin_linter_description =
    json
    |> member "plugin_linter_description"
    |> to_string
  in
  {
    plugin_name = plugin_name;
    plugin_description = plugin_description;
    plugin_linter_name = plugin_linter_name;
    plugin_linter_description = plugin_linter_description
  }

let json_of_plugins_database_entries entries =
  `List (List.map json_of_plugins_database_entry entries)

let plugins_database_entries_of_json json  =
  json |> to_list |> List.map plugins_database_entry_of_json
