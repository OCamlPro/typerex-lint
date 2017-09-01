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
open Web_file_content_data
open Web_errors

let warning_content_code_view_header warning_info =
  div
    ~a:[
      a_class ["panel-heading"];
    ]
    [
      a
        ~a:[
          a_href (Web_utils.file_href warning_info.warning_file);
        ]
        [pcdata warning_info.warning_file.file_name]
    ]

let warning_content_code_view_body warning_info =
  div
    ~a:[
      a_class ["panel-body"];
    ]
    [
      Web_components.warning_code_viewer warning_info;
    ]

let warning_content_code_view warning_info =
  div
    ~a:[
      a_class ["panel"; "panel-default"];
    ]
    [
      warning_content_code_view_header warning_info;
      warning_content_code_view_body warning_info;
    ]

let warning_content warning_info =
  let warning_desc =
    Printf.sprintf "Warning #%d :"
      warning_info.warning_id
  in
  let linter_desc =
    Printf.sprintf "Linter %s : %s"
      warning_info.warning_linter.linter_name
      warning_info.warning_linter.linter_description
  in
  let warning_output_desc =
    Printf.sprintf "%s : %s"
      warning_info.warning_type.decl.short_name
      warning_info.warning_type.output
  in
  let code_view =
    if Web_utils.warning_is_ghost warning_info then
      Web_utils.html_empty_node ()
    else
      div
        [
          br ();
          warning_content_code_view warning_info;
        ]
  in
  div
    ~a:[
      a_class ["hideable-content"];
    ]
    [
      h3 [pcdata warning_desc];
      br ();
      br ();
      h4 [pcdata linter_desc];
      br ();
      h4 [pcdata warning_output_desc];
      code_view;
    ]

let error_content error_info =
  let error_desc =
    Printf.sprintf "Error #%d :"
      error_info.error_id
  in
  let output_desc =
    Printf.sprintf "%s : %s"
      (Web_utils.error_type error_info)
      (Web_utils.error_description error_info)
  in
  div
    ~a:[
      a_class ["hideable-content"];
    ]
    [
      h3 [pcdata error_desc];
      br ();
      h4 [pcdata output_desc];
    ]

let file_code_viewer_header file_info =
  div
    ~a:[
      a_class ["panel-heading"];
    ]
    [
      a
       ~a:[
          a_href (Web_utils.file_href file_info);
        ]
        [pcdata file_info.file_name]
    ]

let file_code_viewer_body file_info =
  div
    ~a:[
      a_class ["panel-body"];
    ]
    [
      Web_components.file_code_viewer file_info;
    ]

let file_code_viewer file_info =
  div
    ~a:[
      a_class ["panel"; "panel-default"];
    ]
    [
      file_code_viewer_header file_info;
      file_code_viewer_body file_info;
    ]

let all_file_content file_info =
  div
    ~a:[
      a_class ["hideable-content"];
    ]
    [
      h3 [pcdata "All file"];
      br ();
      file_code_viewer file_info;
    ]

let alterable_panel_content_creator file_info = function
  | File_content ->
     all_file_content file_info
  | Warning_content warning_info ->
     if not (Web_utils.file_equals warning_info.warning_file file_info) then
       raise (Web_exception
                (Open_warning_from_bad_file (warning_info, file_info)))
     ;
     warning_content warning_info
  | Error_content error_info ->
     if not (Web_utils.file_equals error_info.error_file file_info) then
       raise (Web_exception
                (Open_error_from_bad_file (error_info, file_info)))
     ;
     error_content error_info

let warnings_dropdown file_content_data grid =
  let on_select warning =
    Web_filter_system.remove_filter
      file_content_data.file_content_warnings_filters
      (Web_filter_system.Warning_type_filter warning)
    ;
    Web_filter_system.eval_filters
      file_content_data.file_content_warnings_filters
  in
  let on_deselect warning =
    Web_filter_system.add_warning_filter
      file_content_data.file_content_warnings_filters
      (Web_filter_system.Warning_type_filter warning)
    ;
    Web_filter_system.eval_filters
      file_content_data.file_content_warnings_filters
  in
  let selections =
    List.map begin fun warning_info ->
      Web_components.dropdown_checkbox_selection
        warning_info
        (Web_utils.warning_name warning_info)
        on_select
        on_deselect
    end (Web_utils.warnings_set file_content_data.file_content_warnings_info)
  in
  Web_components.dropdown_menu "warnings" selections grid

let severity_dropdown file_content_data grid =
  let active_class = Js.string "dropdown-selection-active" in
  let previous_severity = ref None in
  let on_click severity label =
    begin match !previous_severity with
    | Some (svt, lbl) ->
       Web_filter_system.remove_filter
         file_content_data.file_content_warnings_filters
         (Web_filter_system.Warning_higher_severity_filter svt)
       ;
       lbl##classList##remove (active_class)
    | None ->
       ()
    end;
    Web_filter_system.add_warning_filter
      file_content_data.file_content_warnings_filters
      (Web_filter_system.Warning_higher_severity_filter severity)
    ;
    label##classList##add (active_class);
    previous_severity := Some (severity, label);
    Web_filter_system.eval_filters
      file_content_data.file_content_warnings_filters
  in
  let selections =
    List.map begin fun severity ->
      Web_components.dropdown_simple_selection
        severity
        (string_of_int severity)
        on_click
    end [1;2;3;4;5;6;7;8;9;10]
  in
  Web_components.dropdown_menu "severity" selections grid

let warning_filter_searchbox file_content_data grid =
  Web_components.searchbox
    file_content_data.file_content_warnings_filters
    (fun kwd -> Web_filter_system.Warning_keyword_filter kwd)
    Web_filter_system.value_of_warning_filter
    grid

let warnings_filter file_content_data =
  div
    ~a:[
      a_class ["dashboard-filter"; "row"];
    ]
    [
      warnings_dropdown
        file_content_data
        ["col-md-1"; "row-vertical-center"];
      severity_dropdown
        file_content_data
        ["col-md-1"; "row-vertical-center"];
      warning_filter_searchbox
        file_content_data
        [
          "col-md-2";
          "col-md-offset-8";
          "col-no-padding";
          "row-vertical-center";
        ];
    ]

let warnings_table_col_id warning_info =
  pcdata (string_of_int warning_info.warning_id)

let warnings_table_col_plugin warning_info =
  pcdata warning_info.warning_linter.linter_plugin.plugin_name

let warnings_table_col_linter warning_info =
  pcdata warning_info.warning_linter.linter_name

let warnings_table_col_warning warning_info =
  pcdata warning_info.warning_type.decl.short_name

let warnings_table_col_severity warning_info =
  pcdata (string_of_int warning_info.warning_type.decl.severity)

let warnings_table_entry warning_info file_content_data =
  let tr =
    tr
      [
        td [warnings_table_col_id warning_info];
        td [warnings_table_col_plugin warning_info];
        td [warnings_table_col_linter warning_info];
        td [warnings_table_col_warning warning_info];
        td [warnings_table_col_severity warning_info];
      ]
  in
  (Tyxml_js.To_dom.of_element tr)##onclick <-Dom_html.handler begin fun _ ->
    focus_file_content file_content_data (Warning_content warning_info);
    Js._true
  end;
  Web_filter_system.register_element
    file_content_data.file_content_warnings_filters
    warning_info
    tr
  ;
  tr

let warnings_table_head () =
  thead
    [
      tr
        [
          th [pcdata "Identifier"];
          th [pcdata "Plugin"];
          th [pcdata "Linter"];
          th [pcdata "Warning"];
          th [pcdata "Severity"];
        ]
    ]

let warnings_table file_content_data =
  let entry_creator warning_info =
    warnings_table_entry warning_info file_content_data
  in
  let table =
    tablex
      ~a:[
        a_class [
            "file-content-table";
            "warnings-file-content-table";
            "col-md-12";
          ];
      ]
      ~thead:(warnings_table_head ())
      [
        tbody (
            List.map entry_creator file_content_data.file_content_warnings_info
          );
      ]
  in
  div
    ~a:[
      a_class ["row"];
    ]
    [table]

let errors_dropdown file_content_data grid =
  let on_select error =
    Web_filter_system.remove_filter
      file_content_data.file_content_errors_filters
      (Web_filter_system.Error_type_filter error)
    ;
    Web_filter_system.eval_filters
      file_content_data.file_content_errors_filters
  in
  let on_deselect error =
    Web_filter_system.add_error_filter
      file_content_data.file_content_errors_filters
      (Web_filter_system.Error_type_filter error)
    ;
    Web_filter_system.eval_filters
      file_content_data.file_content_errors_filters
  in
  let selections =
    List.map begin fun error_info ->
      Web_components.dropdown_checkbox_selection
        error_info
        (Web_utils.error_type error_info)
        on_select
        on_deselect
    end (Web_utils.errors_set file_content_data.file_content_errors_info)
  in
  Web_components.dropdown_menu "errors" selections grid

let error_filter_searchbox file_content_data grid =
  Web_components.searchbox
    file_content_data.file_content_errors_filters
    (fun kwd -> Web_filter_system.Error_keyword_filter kwd)
    Web_filter_system.value_of_error_filter
    grid

let errors_filter file_content_data =
  div
    ~a:[
      a_class ["dashboard-filter"; "row"];
    ]
    [
      errors_dropdown
        file_content_data
        ["col-md-1"; "row-vertical-center"];
      error_filter_searchbox
        file_content_data
        [
          "col-md-2";
          "col-md-offset-9";
          "col-no-padding";
          "row-vertical-center";
        ];
    ]

let errors_table_col_id error_info =
  pcdata (string_of_int error_info.error_id)

let errors_table_col_error error_info =
  pcdata (Web_utils.error_type error_info)

let errors_table_entry error_info file_content_data =
  let tr =
    tr
      [
        td [errors_table_col_id error_info];
        td [errors_table_col_error error_info];
      ]
  in
  (Tyxml_js.To_dom.of_element tr)##onclick <-Dom_html.handler begin fun _ ->
    focus_file_content file_content_data (Error_content error_info);
    Js._true
  end;
  Web_filter_system.register_element
    file_content_data.file_content_errors_filters
    error_info
    tr
  ;
  tr

let errors_table_head () =
  thead
    [
      tr
        [
          th [pcdata "Identifier"];
          th [pcdata "Error"];
        ]
    ]

let errors_table file_content_data =
  let entry_creator error_info =
    errors_table_entry error_info file_content_data
  in
  let table =
    tablex
      ~a:[
        a_class [
            "file-content-table";
            "errors-file-content-table";
            "col-md-12";
          ];
      ]
      ~thead:(errors_table_head ())
      [
        tbody (
            List.map entry_creator file_content_data.file_content_errors_info
          );
      ]
  in
  div
    ~a:[
      a_class ["row"];
    ]
    [table]

let warnings_summary_content file_content_data =
  div
    [
      br ();
      br ();
      warnings_filter file_content_data;
      br ();
      warnings_table file_content_data;
    ]

let warnings_summary_content_empty () =
  div
    [
      br ();
      h4 [pcdata "No provided warnings in this file."];
    ]

let errors_summary_content file_content_data =
  div
    [
      br ();
      br ();
      errors_filter file_content_data;
      br ();
      errors_table file_content_data;
    ]

let errors_summary_content_empty () =
  div
    [
      br ();
      h4 [pcdata "No provided errors in this file."];
    ]

let errors_summary file_content_data =
  let content =
    if Web_utils.list_is_empty file_content_data.file_content_errors_info then
      errors_summary_content_empty ()
    else
      errors_summary_content file_content_data
  in
  let hideable_menu =
    Web_components.hideable_menu_create "All errors" content
  in
  Web_components.hideable_menu_div_element hideable_menu

let content_head file_content_data hideable_warnings_menu hideable_errors_menu =
  let warnings_button =
    button
      ~a:[
        a_button_type `Button;
        a_class ["btn"; "btn-warning"];
      ]
      [pcdata "Show warnings"]
  in
  let errors_button =
    button
      ~a:[
        a_button_type `Button;
        a_class ["btn"; "btn-danger"];
      ]
      [pcdata "Show errors"]
  in
  let all_file_button =
    button
      ~a:[
        a_button_type `Button;
        a_class ["btn"; "btn-info"];
      ]
      [pcdata "See all file"]
  in
  (Tyxml_js.To_dom.of_element warnings_button)##onclick <-Dom_html.handler
  begin fun _ ->
    if not (Web_components.hideable_menu_is_open hideable_warnings_menu) then
      Web_components.hideable_menu_open hideable_warnings_menu
    ;
    Js._true
  end;
  (Tyxml_js.To_dom.of_element errors_button)##onclick <-Dom_html.handler
  begin fun _ ->
    if not (Web_components.hideable_menu_is_open hideable_errors_menu) then
      Web_components.hideable_menu_open hideable_errors_menu
    ;
    Js._true
  end;
  (Tyxml_js.To_dom.of_element all_file_button)##onclick <-Dom_html.handler
  begin fun _ ->
    focus_file_content file_content_data File_content;
    Js._true
  end;
  div
    ~a:[
      a_class ["row"; "content-head"];
    ]
    [
      div
        ~a:[
          a_class ["col-md-6"; "row-vertical-center"];
        ]
        [
          h2 [pcdata file_content_data.file_content_info.file_name];
        ];
      div
        ~a:[
          a_class ["col-md-2"; "row-vertical-center"];
        ]
        [
          warnings_button;
        ];
      div
        ~a:[
          a_class ["col-md-2"; "row-vertical-center"];
        ]
        [
          errors_button;
        ];
      div
        ~a:[
          a_class ["col-md-2"; "row-vertical-center"];
        ]
        [
          all_file_button;
        ];
    ]

let content navigation_system file_content_data =
  let content_div =
    Tyxml_js.Of_dom.of_element
      (file_content_data.file_content_container)
  in
  let warnings_content =
    if Web_utils.list_is_empty file_content_data.file_content_warnings_info then
      warnings_summary_content_empty ()
    else
      warnings_summary_content file_content_data
  in
  let hideable_warnings_menu =
    Web_components.hideable_menu_create "All warnings" warnings_content
  in
  let errors_content =
    if Web_utils.list_is_empty file_content_data.file_content_errors_info then
      errors_summary_content_empty ()
    else
      errors_summary_content file_content_data
  in
  let hideable_errors_menu =
    Web_components.hideable_menu_create "All errors" errors_content
  in
  let separator () =
    div
      ~a:[
        a_class ["row"];
      ]
      [
        div
          ~a:[
            a_class ["col-md-12"; "horizontal-separator"];
          ]
          [];
      ]
  in
  div
    ~a:[
      a_class ["container"];
    ]
    [
      content_head
        file_content_data
        hideable_warnings_menu
        hideable_errors_menu;
      br ();
      br ();
      separator ();
      br ();
      br ();
      content_div;
      br ();
      br ();
      Web_components.hideable_menu_div_element hideable_warnings_menu;
      br ();
      br ();
      Web_components.hideable_menu_div_element hideable_errors_menu;
    ]
