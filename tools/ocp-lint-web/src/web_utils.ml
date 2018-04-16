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

let is_ghost_loc loc =
  let open Location in
  let open Lexing in
  loc.loc_ghost || (loc.loc_start.pos_lnum = 0 && loc.loc_start.pos_cnum = -1)

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

let list_is_empty = function
  | [] -> true
  | _ -> false

let string_contains_keyword ~kwd ~str =
  let re = Regexp.regexp (kwd) in
  match Regexp.search_forward re str 0 with
  | Some _ -> true
  | None -> false

let string_overflow nbchar str =
  let len = String.length str in
  if len <= nbchar then
    str
  else
    let ovr_str = "..." in
    let ovr_len = String.length ovr_str in
    ovr_str ^ (String.sub str (len - nbchar + ovr_len) (nbchar - ovr_len))

let value_of_optional = function
  | Some x -> x
  | None -> raise (Web_exception Get_value_of_empty_optional)

let value_of_js_option opt =
  Js.Opt.get opt (fun () -> raise (Web_exception Get_value_of_empty_optional))

let html_empty_node () =
  Tyxml_js.Html.pcdata ""

let dom_node_remove node =
  ignore ((value_of_js_option node##parentNode)##removeChild (node))

let dom_element_is_display e =
  e##style##display != Js.string "none"

let dom_element_display e =
  e##style##display <- (Js.string "")

let dom_element_undisplay e =
  e##style##display <- (Js.string "none")

let get_element_by_id id =
  match Js_utils.Manip.by_id id with
  | Some element -> element
  | None -> raise (Web_exception (No_such_element_with_id id))

let json_from_js_var var =
  let (str : Js.js_string Js.t) = Js.Unsafe.variable var in
  Yojson.Basic.from_string (Js.to_string str)

let file_equals f f' =
  f.file_hash = f'.file_hash

let file_compare f f' =
  String.compare f.file_hash f'.file_hash

let file_short_name file_info =
  let re = Regexp.regexp (Filename.dir_sep) in
  let dirs = Regexp.split re file_info.file_name in
  List.hd (List.rev dirs)

let file_href file_info =
  Printf.sprintf "%s.html"
    (Lint_web.file_info_page file_info)

let file_warning_href warning_info =
  Printf.sprintf "%s.html#%d"
    (Lint_web.file_info_page warning_info.warning_file)
    warning_info.warning_id

let filename_overflow nbchar filename =
  let len = String.length filename in
  if len <= nbchar then
    filename
  else
    let ovr_str = "..." in
    let ovr_len = String.length ovr_str in
    let sep_len = String.length Filename.dir_sep in
    let maxlen = nbchar - ovr_len - sep_len in
    let re = Regexp.regexp (Filename.dir_sep) in
    let dirs = List.rev (Regexp.split re filename) in
    let res,_ =
      List.fold_left begin fun (acc,isended) p ->
        if isended
           || String.length acc + String.length p + sep_len > maxlen
        then
          (acc,true)
        else
          (p ^ Filename.dir_sep ^ acc, false)
      end (List.hd dirs, false) (List.tl dirs)
    in
    ovr_str ^ Filename.dir_sep ^ res

module FileInfo = struct

  type t = file_info

  let compare = file_compare

end

module FileInfoSet = Set.Make(FileInfo)

let files_set files_info =
  FileInfoSet.elements (
      List.fold_left begin fun acc file ->
        FileInfoSet.add file acc
       end FileInfoSet.empty files_info
    )

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

let linter_name ~pname ~lname =
  Printf.sprintf "%s/%s" pname lname

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
  Printf.sprintf "%s/%s/%s"
    warning_info.warning_linter.linter_plugin.plugin_name
    warning_info.warning_linter.linter_name
    warning_info.warning_type.decl.short_name

let warning_is_ghost warning_info =
  let open Location in
  let open Lexing in
  let loc = warning_info.warning_type.loc in
  let ghost_warnings = [
    "plugin_file_system",
    "interface_missing",
    "missing_interface"
  ;
    "plugin_parsetree",
    "code_redefine_stdlib_module",
    "redfine_compilerlib_module"
  ;
    "plugin_text",
    "useless_space_line",
    "useless_line"
  ;
  ]
  in
  let is_same_warning (plugin_name,linter_name,warning_name) =
    warning_info.warning_linter.linter_plugin.plugin_name = plugin_name
    && warning_info.warning_linter.linter_name = linter_name
    && warning_info.warning_type.decl.short_name = warning_name
  in
  List.exists is_same_warning ghost_warnings && is_ghost_loc loc

module WarningInfo = struct

  type t = warning_info

  let compare = warning_compare

end

module WarningInfoSet = Set.Make(WarningInfo)

let warnings_set warnings_info =
  WarningInfoSet.elements (
      List.fold_left begin fun acc warning ->
        WarningInfoSet.add warning acc
       end WarningInfoSet.empty warnings_info
    )

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
     Lint_plugin_error.to_string e
  | Lint_db_types.Sempatch_error e ->
     e
  | Lint_db_types.Ocplint_error e ->
     e

let error_equals e e' =
  (error_type e) = (error_type e')

let error_compare e e' =
  String.compare (error_type e) (error_type e')

module ErrorInfo = struct

  type t = error_info

  let compare = error_compare

end

module ErrorInfoSet = Set.Make(ErrorInfo)

let errors_set errors_info =
  ErrorInfoSet.elements (
      List.fold_left begin fun acc error ->
        ErrorInfoSet.add error acc
       end ErrorInfoSet.empty errors_info
    )
