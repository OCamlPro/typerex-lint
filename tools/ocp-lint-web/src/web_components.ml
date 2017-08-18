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

let searchbox
      filter_system filter_type_creator filter_value_of_id_creator grid =
  let searchbox_input =
    input
      ~a:[
        a_input_type `Text;
        a_class ["form-control"; "filter-searchbox"];
        a_placeholder "Search..."
      ] ()
  in
  let clear_button =
    span
      ~a:[
        a_class [
            "form-control-clear";
            "form-control-feedback";
          ];
        a_style "display:none";
      ]
      [pcdata "×"]
  in
  let searchbox_dom = Tyxml_js.To_dom.of_input searchbox_input in
  let clear_button_dom = Tyxml_js.To_dom.of_element clear_button in
  let get_input () = Js.to_string (searchbox_dom##value) in
  let previous_filter = ref None in
  clear_button_dom##onclick <- Dom_html.handler begin fun _ ->
    (* todo *)
    Js._true
  end;
  searchbox_dom##onkeyup <- Dom_html.handler begin fun _ ->
    let input = get_input () in
    if input = "" && Web_utils.dom_element_is_display clear_button_dom then
      Web_utils.dom_element_undisplay clear_button_dom
    else if input != ""
            && not (Web_utils.dom_element_is_display clear_button_dom) then
      Web_utils.dom_element_display clear_button_dom
    ;
    let filter =
      if input = "" then
        None
      else
        Some (filter_type_creator input)
    in
    begin match !previous_filter with
    | Some fltr ->
       Web_filter_system.remove_filter filter_system fltr
    | None ->
       ()
    end;
    begin match filter with
    | Some fltr ->
       Web_filter_system.add_filter
         filter_system
         fltr
         (filter_value_of_id_creator fltr)
    | None ->
       ()
    end;
    previous_filter := filter;
    Web_filter_system.eval_filters filter_system;
    Js._true
  end;
  div
    ~a:[
      a_class (["input-group"] @ grid);
    ]
    [
      div
        ~a:[
          a_class ["form-group"; "has-feedback"; "has-clear"];
        ]
        [
          searchbox_input;
          clear_button;
        ]
    ]

let warning_box_head warning_info =
  h4
    ~a:[
      a_class ["alert-heading"];
    ]
    [pcdata (Printf.sprintf "Warning #%d" warning_info.warning_id)]

let warning_box_body warning_info =
  let file_msg =
    span
      ~a:[
        a_class ["alert-link"];
      ]
      [
        pcdata warning_info.warning_file.file_name;
      ]
  in
  let line_msg =
    let str =
      let open Web_utils in
      match file_loc_of_warning_info warning_info with
      | Floc_line line ->
         Printf.sprintf "line %d" line
      | Floc_lines_cols (bline, _, eline, _) ->
         if bline = eline then
           Printf.sprintf "line %d" bline
         else
           Printf.sprintf "line %d to %d" bline eline
    in
    pcdata str
  in
  let linter_msg =
    pcdata (
      Printf.sprintf "raised by %s"
        (Web_utils.linter_name
           warning_info.warning_linter.linter_plugin.plugin_name
           warning_info.warning_linter.linter_name)
    )
  in
  let warning_msg =
    pcdata (
      Printf.sprintf "%s : %s"
        warning_info.warning_type.decl.short_name
        warning_info.warning_type.output
    )
  in
  div
    [
      span
        ~a:[
          a_class
            [
              "col-md-1";
              "row-vertical-center";
              "glyphicon";
              "glyphicon-alert";
            ];
        ]
        [];
      div
        ~a:[
          a_class ["col-md-11"; "row-vertical-center"];
        ]
        [
          pcdata "from ";
          file_msg;
          pcdata " ";
          line_msg;
          br ();
          warning_msg;
          br ();
          linter_msg
        ];
    ]

let warning_box warning_info onclick =
  let div_warning =
    div
    ~a:[
      a_class ["alert"; "alert-warning"; "row"];
    ]
    [
      warning_box_head warning_info;
      br ();
      warning_box_body warning_info;
    ]
  in
  (Tyxml_js.To_dom.of_element div_warning)##onclick <- Dom_html.handler
  begin fun _ ->
    onclick ();
    Js._true
  end;
  div_warning

let error_box_head error_info =
  h4
    ~a:[
      a_class ["alert-heading"];
    ]
    [pcdata (Printf.sprintf "Error #%d" error_info.error_id)]

let error_box_body error_info =
  let file_msg =
    span
      ~a:[
        a_class ["alert-link"];
      ]
      [
        pcdata error_info.error_file.file_name;
      ]
  in
  let description_msg =
    pcdata (
      Printf.sprintf "%s : %s"
        (Web_utils.error_type error_info)
        (Web_utils.error_description error_info)
    )
  in
  div
    [
      span
        ~a:[
          a_class
            [
              "col-md-1";
              "row-vertical-center";
              "glyphicon";
              "glyphicon-remove-sign";
            ];
        ]
        [];
      div
        ~a:[
          a_class ["col-md-11"; "row-vertical-center"];
        ]
        [
          pcdata "from ";
          file_msg;
          br ();
          description_msg;
        ];
    ]

let error_box error_info onclick =
  let div_error =
    div
    ~a:[
      a_class ["alert"; "alert-danger"; "row"];
    ]
    [
      error_box_head error_info;
      br ();
      error_box_body error_info;
    ]
  in
  (Tyxml_js.To_dom.of_element div_error)##onclick <- Dom_html.handler
  begin fun _ ->
    onclick ();
    Js._true
  end;
  div_error
