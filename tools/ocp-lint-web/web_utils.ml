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

(* todo better place *)

type file_content_type =
  | File_content
  | Warning_content of warning_info

type file_content_value = {
  dom_content : Dom_html.element Js.t;
  mutable is_active : bool;
}

type file_content_warning_data = {
  file_content_warning_info : warning_info;
  file_content_warning_element : Dom_html.element Js.t
}

type file_content_filter_id =
  | Warning_type_filter of string
  | Keyword_filter

type file_content_data = {
  file_content_info :
    file_info;
  mutable file_content_warnings:
    file_content_warning_data list;
  file_content_container :
    Dom_html.element Js.t;
  file_content_main_contents :
    (file_content_type, file_content_value) Hashtbl.t;
  file_content_creator :
    file_content_type -> Dom_html.element Js.t;
  file_content_filters :
    (file_content_filter_id, warning_info -> bool) Hashtbl.t;
}

let create_file_content_data file_info dom_content_container content_creator =
  {
    file_content_info = file_info;
    file_content_warnings = [];
    file_content_container = Tyxml_js.To_dom.of_element dom_content_container;
    file_content_main_contents = Hashtbl.create 64;
    file_content_creator = content_creator;
    file_content_filters = Hashtbl.create 16;
  }

let register_file_content_warning_data
      file_content_data warning_info dom_element =
  let warning_data =
    {
      file_content_warning_info = warning_info;
      file_content_warning_element = dom_element;
    }
  in
  file_content_data.file_content_warnings <-
    warning_data :: file_content_data.file_content_warnings

let add_file_content_filter file_content_data filter_id filter =
  Hashtbl.add file_content_data.file_content_filters filter_id filter

let remove_file_content_filter file_content_data filter_id =
  Hashtbl.remove file_content_data.file_content_filters filter_id

let eval_file_content_filters file_content_data = 
  let full_filter warning =
    Hashtbl.fold begin fun _ filter acc ->
      acc && filter warning
    end file_content_data.file_content_filters true
  in
  List.iter begin fun warning_data ->
    if full_filter warning_data.file_content_warning_info then
      (* Web_utils.dom_element_display warning_data.file_content_warning_element *)
      warning_data.file_content_warning_element##style##display <- (Js.string "")
    else
      (* Web_utils.dom_element_undisplay warning_data.file_content_warning_element *)
      warning_data.file_content_warning_element##style##display <- (Js.string "none")
  end file_content_data.file_content_warnings
	    
let active_file_content file_content_data =
  Hashtbl.fold begin fun _ content_value acc ->
    if content_value.is_active then
      match acc with
      | Some _ -> failwith "" (* todo web err *)
      | None -> Some content_value
    else
      acc
  end file_content_data.file_content_main_contents None

let focus_file_content file_content_data file_content_type =
  let file_content_warning =
    try
      Hashtbl.find
        file_content_data.file_content_main_contents
        file_content_type
    with
    | Not_found ->
       let default_content_value = {
         dom_content = file_content_data.file_content_creator file_content_type;
         is_active = false;
       }
       in
       dom_element_undisplay default_content_value.dom_content;
       Dom.appendChild
	 file_content_data.file_content_container
	 default_content_value.dom_content;
       Hashtbl.add
         file_content_data.file_content_main_contents
         file_content_type
         default_content_value;
       default_content_value
  in
  if not file_content_warning.is_active then begin
    let active = active_file_content file_content_data in
    begin match active with
      | Some content ->
         dom_element_undisplay content.dom_content;
         content.is_active <- false 
      | _ -> ()
    end;
    dom_element_display file_content_warning.dom_content;
    file_content_warning.is_active <- true
  end

(* -------------------- *)
