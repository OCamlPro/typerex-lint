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

open Tyxml_js.Html
open Lint_warning_types
open Lint_web_analysis_info
open Web_errors

type file_loc =
  | Floc_line of int
  | Floc_lines_cols of int * int * int * int

let is_ghost_loc loc =
  let open Location in
  let open Lexing in
  (* loc.loc_ghost  && (loc.loc_start.pos_lnum = 0) *)
  (* todo check *)
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

let warning_info_is_ghost warning_info =
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
  ]
  in
  let is_same_warning (plugin_name,linter_name,warning_name) =
    warning_info.warning_linter.linter_plugin.plugin_name = plugin_name
    && warning_info.warning_linter.linter_name = linter_name
    && warning_info.warning_type.decl.short_name = warning_name
  in
  List.exists is_same_warning ghost_warnings && is_ghost_loc loc

let get_element_by_id id =
  match Js_utils.Manip.by_id id with
  | Some element -> element
  | None -> raise (Web_exception (No_such_element_with_id id))

let list_is_empty = function
  | [] -> true
  | _ -> false

let string_overflow nbchar str =
  let len = String.length str in
  if len <= nbchar then
    str
  else
    let ovr_str = "..." in
    let ovr_len = String.length ovr_str in
    Firebug.console##log (len);
    Firebug.console##log (len - nbchar - 1);
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

let file_compare f f' =
  String.compare f.file_hash f'.file_hash

let file_short_name file_info =
  let re = Regexp.regexp (Filename.dir_sep) in
  let dirs = Regexp.split re file_info.file_name in
  try
    List.hd (List.rev dirs)
  with
    Failure _ -> raise (Web_exception (Invalid_file_name file_info))

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

let linter_name linter_info =
  Printf.sprintf "%s/%s"
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
  Printf.sprintf "%s/%s/%s"
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

let error_equals e e' =
  (error_type e) = (error_type e')

let error_compare e e' =
  String.compare (error_type e) (error_type e')

let error_contains_keyword keyword error_info =
  let re = Regexp.regexp (keyword) in
  let contains_kwd str =
    match Regexp.search_forward re str 0 with
    | Some _ -> true
    | None -> false
  in
  contains_kwd (error_type error_info)
  || contains_kwd (error_description error_info)
  || contains_kwd error_info.error_file.file_name

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

let code_viewer_line_size =
  17

let code_viewer_context_line_number =
  3

let code_viewer_begin_context_from_line line_number =
  min (line_number - 1) code_viewer_context_line_number

let code_viewer_end_context_from_line line_number lines_count =
  min (lines_count - line_number) code_viewer_context_line_number

let code_viewer line_number href =
  let height = (* todo min height *)
    code_viewer_line_size * (line_number + 2)
  in
  Tyxml_js.Html.iframe
    ~a:[
      Tyxml_js.Html.a_src href;
      Tyxml_js.Html.a_style ("height: " ^ (string_of_int height) ^ "px");
    ]
    []

let file_code_viewer file_info =
  code_viewer
    (file_info.file_lines_count)
    (file_href file_info)

let warning_code_viewer warning_info =
  let begin_line, end_line =
    match file_loc_of_warning_info warning_info with
    | Floc_line line ->
       line, line
    | Floc_lines_cols (bline, _, eline, _) ->
       bline, eline
  in
  let lines_count = warning_info.warning_file.file_lines_count in
  let begin_with_context =
    begin_line - code_viewer_begin_context_from_line begin_line
  in
  let end_with_context =
    end_line + code_viewer_end_context_from_line end_line lines_count
  in
  code_viewer
    (end_with_context - begin_with_context)
    (file_warning_href warning_info)

let filter_dropdown_simple_selection value label_value on_click =
  let severity_selection =
    a
      [
        label
	  ~a:[
            a_class ["filter-label";];
	  ]
	  [
            pcdata label_value;
	  ]
      ]
  in
  let dom_selection = Tyxml_js.To_dom.of_a severity_selection in
  dom_selection##onclick <- Dom_html.handler begin fun _ ->
    on_click value dom_selection;
    Js._true
  end;
  li
    [
      severity_selection;
    ]

let filter_dropdown_checkbox_selection value label_value on_select on_deselect =
  let checkbox =
    input
      ~a:[
        a_class ["filter-checkbox"];
        a_input_type `Checkbox;
        a_checked ();
      ] ();
  in
  let dom_checkbox = Tyxml_js.To_dom.of_input checkbox in
  let is_selected () = Js.to_bool dom_checkbox##checked in
  dom_checkbox##onclick <- Dom_html.handler begin fun _ ->
    if is_selected () then
      on_select value
    else
      on_deselect value
    ;
    Js._true
  end;
  li
    [
      a
        [
	  checkbox;
          label
            ~a:[
              a_class ["filter-label"];
            ]
            [
              pcdata label_value;
            ];
	]
    ]

let filter_dropdown_menu label_value dropdown_selections grid =
  div
    ~a:[
      a_class (["dropdown"] @ grid);
    ]
    [
      button
        ~a:[
          a_class ["btn"; "btn-default"; "dropdown-toggle"];
          a_button_type `Button;
          a_user_data "toggle" "dropdown";
        ]
        [
          pcdata (label_value ^ " "); (* todo change *)
          span ~a:[a_class ["caret"]] [];
        ];
      ul
        ~a:[
          a_class ["dropdown-menu"; "scrollable-menu"];
        ]
        dropdown_selections;
      ]

