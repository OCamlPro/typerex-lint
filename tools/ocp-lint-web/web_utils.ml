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

open Lint_warning_types
open Lint_web_warning
open Lint_web_plugin
open Web_errors

type file_loc =
  | Floc_line of int
  | Floc_lines_cols of int * int * int * int
					   
let file_loc_of_loc loc =
  let open Location in
  let open Lexing in
  let col_of_pos pos = pos.pos_cnum - pos.pos_bol in
  if not loc.loc_ghost then
    Some (Floc_lines_cols (
	loc.loc_start.pos_lnum,
	col_of_pos loc.loc_start,
	loc.loc_end.pos_lnum,
	col_of_pos loc.loc_end
      ))
  else if loc.loc_start.pos_lnum != 0 then
    Some (Floc_line loc.loc_start.pos_lnum)
  else
    None

let warning_location_is_ghost warning_entry =
  let open Location in
  let open Lexing in
  let loc = warning_entry.warning_result.loc in
  let ghost_warnings = [
    "plugin_file_system", "interface_missing", "missing_interface";
  ]
  in
  (List.exists begin fun (plugin,linter,warning) ->
    warning_entry.warning_plugin_name = plugin
    && warning_entry.warning_linter_name = linter
    && warning_entry.warning_result.decl.short_name = warning
   end ghost_warnings) || (loc.loc_ghost && (loc.loc_start.pos_lnum = 0))
      
let get_element_by_id id =
  match Js_utils.Manip.by_id id with
  | Some element -> element
  | None -> raise (Web_exception (No_such_element_with_id id))

let list_joining join lst =
  let hd = List.hd lst in
  let tl = List.tl lst in
  List.fold_left begin fun acc line ->
    acc ^ join ^ line 
  end hd tl 
		     
let array_joining join arr =
  let hd = arr.(0) in
  let tl = Array.sub arr 1 ((Array.length arr) - 1) in
  Array.fold_left begin fun acc line ->
    acc ^ join ^ line 
  end hd tl 

let file_href warning_entry =
    (Lint_web.web_static_gen_file warning_entry.warning_hash)
    ^ ".html"
		  
let warning_href warning_entry =
    (Lint_web.web_static_gen_file warning_entry.warning_hash)
    ^ ".html#"
    ^ (string_of_int warning_entry.warning_id)

let json_from_js_var var =
  let (str : Js.js_string Js.t) = Js.Unsafe.variable var in
  Yojson.Basic.from_string (Js.to_string str)
       
let find_plugin_entry warning_entry plugins_entries =
  List.find begin fun plugin_entry ->
    warning_entry.warning_plugin_name = plugin_entry.plugin_name
    && warning_entry.warning_linter_name = plugin_entry.plugin_linter_name
  end plugins_entries
