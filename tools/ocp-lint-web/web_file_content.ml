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
      Web_code_viewer.file_code_viewer file_info;
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
    [
      h3 [pcdata "All file"];
      br ();
      file_code_viewer file_info;
    ]

let main_content_creator file_info = function
  | File_content ->
     Tyxml_js.To_dom.of_element (all_file_content file_info)
  | Warning_content warning_info ->
     (* todo maybe check if warning.file = file *)
     Tyxml_js.To_dom.of_element
       (* todo warning_content in this file *)
       (Web_warning_content.warning_content warning_info)

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
              a_class ["filter-label";];
            ]
            [
              pcdata label_value;
            ];
	]
    ]

let filter_dropdown_menu label_value dropdown_selections =
  div
    ~a:[
      a_class ["dropdown"];
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
          a_class ["dropdown-menu"];
        ]
        dropdown_selections;
      ]

let warnings_dropdown warnings_info file_content_data =
  let on_select warning =
    remove_file_content_filter file_content_data (Warning_type_filter warning);
    eval_file_content_filters file_content_data
  in
  let on_deselect warning =
    add_file_content_filter file_content_data (Warning_type_filter warning);
    eval_file_content_filters file_content_data
  in
  let selections =
    List.map begin fun warning_info ->
      filter_dropdown_checkbox_selection
        warning_info
        (Web_utils.warning_name warning_info)
        on_select
        on_deselect
    end warnings_info
  in
  filter_dropdown_menu "warnings" selections

let severity_dropdown file_content_data =
  let active_class = Js.string "dropdown-selection-active" in
  let previous_severity = ref None in
  let on_click severity label =
    begin match !previous_severity with
    | Some (svt, lbl) ->
       remove_file_content_filter
	 file_content_data
	 (Higher_severity_filter svt);
       lbl##classList##remove (active_class)
    | None ->
       ()
    end;
    add_file_content_filter file_content_data (Higher_severity_filter severity);
    label##classList##add (active_class);
    previous_severity := Some (severity, label);
    eval_file_content_filters file_content_data;
  in
  let selections =
    List.map begin fun severity ->
      filter_dropdown_simple_selection
        severity
	(string_of_int severity)
	on_click
    end [1;2;3;4;5;6;7;8;9;10]
  in
  filter_dropdown_menu "severity" selections

let filter_searchbox file_content_data =
  let searchbox =
    input
      ~a:[
        a_input_type `Text;
        a_class ["form-control"; "filter-searchbox"];
        a_placeholder "Search..."
      ] ()
  in
  let searchbox_dom = Tyxml_js.To_dom.of_input searchbox in
  let get_keyword () =
    let str = Js.to_string (searchbox_dom##value) in
    if str = "" then
      None
    else
      Some str
  in
  let previous_keyword = ref None in
  searchbox_dom##onkeyup <- Dom_html.handler begin fun _ ->
    let keyword = get_keyword () in
    begin match !previous_keyword with
    | Some kwd ->
       remove_file_content_filter file_content_data (Keyword_filter kwd)
    | None ->
       ()
    end;
    begin match keyword with
    | Some kwd ->
       add_file_content_filter file_content_data (Keyword_filter kwd)
    | None ->
       ()
    end;
    previous_keyword := keyword;
    eval_file_content_filters file_content_data;
    Js._true
  end;
  searchbox

let warnings_filter warnings_info file_content_data =
  let uniq_warnings_info =
    warnings_info
    |> List.sort Web_utils.warning_compare
    |> Web_utils.remove_successive_duplicates Web_utils.warning_equals
  in
  div
    ~a:[
      a_class ["dashboard-filter"];
    ]
    [
      warnings_dropdown uniq_warnings_info file_content_data;
      div ~a:[a_class ["filter-separator"]] [];
      severity_dropdown file_content_data;
      div ~a:[a_class ["filter-separator"]] [];
      filter_searchbox file_content_data;
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
  let dom_tr = Tyxml_js.To_dom.of_element tr in
  dom_tr##onclick <-Dom_html.handler begin fun _ ->
    focus_file_content file_content_data (Warning_content warning_info);
    Js._true
  end;
  register_file_content_warning_data
    file_content_data warning_info
    dom_tr
  ;
  tr

let warnings_table_head =
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

let warnings_table warnings_info file_content_data =
  let entry_creator warning_info =
    warnings_table_entry warning_info file_content_data
  in
  let table =
    tablex
      ~a:[

        a_class ["dyntable"];


        (* setAttribute(Js.string "cellspacing", Js.string "0"); *)
        (* setAttribute(Js.string "width", Js.string "100%"); *)
      ]
      ~thead:warnings_table_head
      [
        tbody (List.map entry_creator warnings_info);
      ]
  in
  (* Web_data_table.set table; *)
  table

let content all_file_warnings_info file_content_data =
  let content_div =
    Tyxml_js.Of_dom.of_element
      (file_content_data.file_content_container)
  in
  let all_file_button =
    button
      ~a:[
        a_button_type `Button;
        a_class ["btn"; "btn-info"];
      ]
      [pcdata "See all file"]
  in
  (Tyxml_js.To_dom.of_element all_file_button)##onclick <-Dom_html.handler
  begin fun _ ->
    focus_file_content file_content_data File_content;
    Js._true
  end;
  div
    [
      h2 [pcdata file_content_data.file_content_info.file_name];
      br ();
      content_div;
      br ();
      warnings_filter all_file_warnings_info file_content_data;
      br ();
      all_file_button;
      br ();
      br ();
      warnings_table all_file_warnings_info file_content_data;
    ]

let new_file_content_data_of_file_info file_info =
  Web_file_content_data.create_file_content_data
    file_info
    (div [])
    (main_content_creator file_info)

let open_tab file_info file_warnings_info =
  let open Web_navigation_system in
  (* todo improve *)
  let file_content_data = new_file_content_data_of_file_info file_info in
  let content = content file_warnings_info file_content_data in
  let attach =
    open_tab
      (FileElement file_info)
      (file_info.file_name)
      (content)
      begin fun () ->
        File_content_attached_data file_content_data
      end
  in
  match attach with
  | File_content_attached_data file_content_data -> file_content_data
  | _ -> failwith "bad attach" (* todo web err *)
