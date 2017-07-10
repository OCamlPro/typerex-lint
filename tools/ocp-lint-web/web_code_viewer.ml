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
open Ace
open Lint_warning_types
open Lint_web_analysis_info
open Web_errors

let theme =
  "monokai"

let font_size =
  14

let line_size =
  17

let context_line_number =
  3

let ace_loc begin_line begin_col end_line end_col =
  {
    loc_start = begin_line, begin_col;
    loc_end = end_line, end_col;
  }

let begin_context_from_line line_number =
  min (line_number - 1) context_line_number

let end_context_from_line line_number lines_count =
  min (lines_count - line_number) context_line_number

let set_default_code_viewer ace =
  Ace.set_mode ace "ace/mode/ocaml";
  Ace.set_theme ace ("ace/theme/" ^ theme);
  Ace.set_font_size ace font_size;
  Ace.set_read_only ace true;
  Ace.set_show_print_margin ace false;
  Ace.set_display_indent_guides ace false;
  Ace.set_highlight_active_line ace false;
  Ace.set_highlight_gutter_line ace false

let set_warning_code_viewer ace warning =
  let begin_line, begin_col, end_line, end_col =
    let open Web_utils in
    match file_loc_of_warning_info warning with
    | Floc_line line ->
       line, 0, line, String.length (Ace.get_line ace (line - 1))
    | Floc_lines_cols (bline, bcol, eline, ecol) ->
       bline, bcol, eline, ecol
  in
  let lines_count = warning.warning_file.file_lines_count in
  let begin_context = begin_context_from_line begin_line in
  let end_context = end_context_from_line end_line lines_count in
  let begin_with_context = begin_line - begin_context in
  let end_with_context = end_line + end_context in
  let loc =
    ace_loc
      (begin_context + 1)
      begin_col
      (begin_context + end_line - begin_line + 1)
      end_col
  in
  let content =
    Web_utils.array_joining
      "\n"
      (Ace.get_lines ace (begin_with_context - 1) (end_with_context - 1))
  in
  Ace.set_value ace content;
  Ace.clear_selection ace;
  Ace.set_first_line_number ace begin_with_context;
  Ace.add_marker ace Ace.Warning loc;
  Ace.set_annotation ace Ace.Warning warning.warning_type.output loc

let set_file_code_viewer ace warnings =
  let warnings =
    Lint_web.group_by begin fun warning_info ->
      let open Web_utils in
      match file_loc_of_warning_info warning_info with
      | Floc_line line ->
         line
      | Floc_lines_cols (line, _, _, _) ->
         line
    end warnings
  in
  List.iter begin fun (line, warnings_info) ->
    let loc = {
      loc_start = line, 0;
      loc_end = line, 0;
    }
    in
    let tooltip =
      (string_of_int (List.length warnings_info))
      ^ " warning(s) : \n - "
      ^ Web_utils.list_joining
          "\n - "
          (List.map (fun warning -> warning.warning_type.output) warnings_info)
    in
    Ace.set_annotation ace Ace.Warning tooltip loc;
    List.iter begin fun warning ->
      let begin_line, begin_col, end_line, end_col =
        let open Web_utils in
        match file_loc_of_warning_info warning with
        | Floc_line line ->
           line, 0, line, String.length (Ace.get_line ace (line - 1))
        | Floc_lines_cols (bline, bcol, eline, ecol) ->
           bline, bcol, eline, ecol
      in
      let loc = ace_loc begin_line begin_col end_line end_col in
      Ace.add_marker ace Ace.Warning loc
    end warnings_info
  end warnings

let init_code_viewer warnings_info id =
  let code_div = Web_utils.get_element_by_id Lint_web.web_code_viewer_id in
  let ace = Ace.create_editor (Tyxml_js.To_dom.of_div code_div) in
  set_default_code_viewer ace;
  match id with
  | Some x ->
     let warning_info =
       try
         List.find begin fun warning ->
           warning.warning_id = x
         end warnings_info
       with
         Not_found ->
           raise (Web_exception (Unknow_warning_id (string_of_int x)))
     in
     set_warning_code_viewer ace warning_info
  | None ->
     let printable_warnings =
       List.filter begin fun warning_info ->
         not (Web_utils.warning_info_is_ghost warning_info)
       end warnings_info
     in
     set_file_code_viewer ace printable_warnings

let onload _ =
  try
    let warnings_info =
      warnings_info_of_json
       (Web_utils.json_from_js_var Lint_web.warnings_info_var)
    in
    let fragment = Url.Current.get_fragment () in
    let id =
      if fragment = "" then
        None
      else
        try
          Some (int_of_string fragment)
        with
          Failure _ -> raise (Web_exception (Unknow_warning_id fragment))
    in
    init_code_viewer warnings_info id;
    Js._true
  with
  | Web_exception e ->
     process_error e;
     Js._false
  | e ->
     failwith ("uncatched exception " ^ (Printexc.to_string e))

let () =
  Dom_html.window##onload <- Dom_html.handler onload

let code_viewer line_number href =
  let height = (* todo min height *)
    line_size * (line_number + 2)
  in
  iframe
    ~a:[
      a_src href;
      a_style ("height: " ^ (string_of_int height) ^ "px");
    ]
    []

let warning_code_viewer warning_info =
  let begin_line, end_line =
    let open Web_utils in
    match file_loc_of_warning_info warning_info with
    | Floc_line line ->
       line, line
    | Floc_lines_cols (bline, _, eline, _) ->
       bline, eline
  in
  let lines_count = warning_info.warning_file.file_lines_count in
  let begin_with_context =
    begin_line - begin_context_from_line begin_line
  in
  let end_with_context =
    end_line + end_context_from_line end_line lines_count
  in
  code_viewer
    (end_with_context - begin_with_context)
    (Web_utils.file_warning_href warning_info)
