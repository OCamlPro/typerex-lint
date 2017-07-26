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
open Lint_web_analysis_info
open Web_errors

type file_loc =
  | Floc_line of int
  | Floc_lines_cols of int * int * int * int

let file_loc_of_warning_info warning_info =
  let open Location in
  let open Lexing in
  let loc = warning_info.warning_type.loc in
  let col_of_pos pos = pos.pos_cnum - pos.pos_bol in
  if not loc.loc_ghost then
    Floc_lines_cols (
        loc.loc_start.pos_lnum,
        col_of_pos loc.loc_start,
        loc.loc_end.pos_lnum,
        col_of_pos loc.loc_end
      )
  else if loc.loc_start.pos_lnum != 0 then
    Floc_line loc.loc_start.pos_lnum
  else
    raise (Web_exception (Ghost_location loc))

let warning_info_is_ghost warning_info =
  let open Location in
  let open Lexing in
  let loc = warning_info.warning_type.loc in
  let ghost_warnings = [
    "plugin_file_system", "interface_missing", "missing_interface";
  ]
  in
  (List.exists begin fun (plugin,linter,warning) ->
    warning_info.warning_linter.linter_plugin.plugin_name = plugin
    && warning_info.warning_linter.linter_name = linter
    && warning_info.warning_type.decl.short_name = warning
   end ghost_warnings) || (loc.loc_ghost && (loc.loc_start.pos_lnum = 0))

let get_element_by_id id =
  match Js_utils.Manip.by_id id with
  | Some element -> element
  | None -> raise (Web_exception (No_such_element_with_id id))

let rec remove_successive_duplicates equals = function
  | [] ->
     []
  | [x] ->
     [x]
  | x :: (y :: tail as l) when equals x y ->
     remove_successive_duplicates equals l
  | x :: tail ->
     x :: remove_successive_duplicates equals tail

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

let dom_element_is_display e =
  e##style##display != Js.string "none"

let dom_element_display e =
  e##style##display <- (Js.string "")

let dom_element_undisplay e =
  e##style##display <- (Js.string "none")

let file_href file_info =
    (generated_static_page_of_file file_info)
    ^ ".html"

let file_warning_href warning_info =
  (generated_static_page_of_file warning_info.warning_file)
  ^ ".html#"
  ^ (string_of_int warning_info.warning_id)

let json_from_js_var var =
  let (str : Js.js_string Js.t) = Js.Unsafe.variable var in
  Yojson.Basic.from_string (Js.to_string str)

let file_equals f f' =
  f.file_hash = f'.file_hash

let plugin_equals p p' =
  p.plugin_name = p'.plugin_name

let plugin_compare p p' =
  String.compare p.plugin_name p'.plugin_name

let linter_equals l l' =
  l.linter_name = l'.linter_name
  && plugin_equals l.linter_plugin l'.linter_plugin

let linter_compare l l' =
  let cmp = String.compare l.linter_name l'.linter_name in
  if cmp != 0 then
    cmp
  else
    plugin_compare l.linter_plugin l'.linter_plugin

let linter_name linter_info =
  Printf.sprintf "%s.%s"
    linter_info.linter_plugin.plugin_name
    linter_info.linter_name

let warning_equals w w' =
  w.warning_type.decl.short_name = w'.warning_type.decl.short_name
  && linter_equals w.warning_linter w'.warning_linter

let warning_compare w w' =
  let cmp =
    String.compare
      w.warning_type.decl.short_name
      w'.warning_type.decl.short_name
  in
  if cmp != 0 then
    cmp
  else
    linter_compare w.warning_linter w'.warning_linter

let warning_name warning_info =
  Printf.sprintf "%s.%s.%s"
    warning_info.warning_linter.linter_plugin.plugin_name
    warning_info.warning_linter.linter_name
    warning_info.warning_type.decl.short_name

let warning_contains_keyword keyword warning_info =
  let re = Regexp.regexp (keyword) in
  let contains_kwd str =
    match Regexp.search_forward re str 0 with
    | Some _ -> true
    | None -> false
  in
  contains_kwd warning_info.warning_file.file_name
  || contains_kwd warning_info.warning_linter.linter_plugin.plugin_name
  || contains_kwd warning_info.warning_linter.linter_name
  || contains_kwd warning_info.warning_type.decl.short_name
  || contains_kwd warning_info.warning_type.output

let error_type error_info =
  match error_info.error_type with
  | Lint_db_types.Db_error e -> "db_error"
  | Lint_db_types.Plugin_error e -> "plugin_error"
  | Lint_db_types.Sempatch_error e -> "sempatch_error"
  | Lint_db_types.Ocplint_error e -> "ocplint_error"

let error_description error_info =
  match error_info.error_type with
  | Lint_db_types.Db_error e ->
     Lint_db_error.to_string e
  | Lint_db_types.Plugin_error e ->
     begin match e with
     | Lint_plugin_error.Plugin_exception (Failure str) ->
        Printf.sprintf "Exception %s" str	  
     | _ ->
        Lint_plugin_error.to_string e
     end
  | Lint_db_types.Sempatch_error e ->
     e
  | Lint_db_types.Ocplint_error e ->
     e
