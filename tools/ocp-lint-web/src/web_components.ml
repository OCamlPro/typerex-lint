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

let code_viewer_line_size =
  17

let code_viewer_context_line_number =
  3

let code_viewer_begin_context_from_line line_number =
  min (line_number - 1) code_viewer_context_line_number

let code_viewer_end_context_from_line ~line_number ~lines_count =
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
    (Web_utils.file_href file_info)

let warning_code_viewer warning_info =
  let begin_line, end_line =
    match Web_utils.file_loc_of_warning_info warning_info with
    | Web_utils.Floc_line line ->
       line, line
    | Web_utils.Floc_lines_cols (bline, _, eline, _) ->
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
    (Web_utils.file_warning_href warning_info)

let dropdown_simple_selection value label_value on_click =
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
  let dom_selection = Tyxml_js.To_dom.of_element severity_selection in
  dom_selection##onclick <- Dom_html.handler begin fun _ ->
    on_click value dom_selection;
    Js._true
  end;
  li
    [
      severity_selection;
    ]

let dropdown_checkbox_selection value label_value on_select on_deselect =
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

let dropdown_menu label_value dropdown_selections grid =
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
