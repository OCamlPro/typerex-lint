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

let filter_dropdown_selection value label_value on_select on_deselect =
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
      label
        ~a:[
          a_class ["filter-label";];
        ]
        [
          checkbox;
          pcdata label_value;
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

let dropdown_creator label label_creator on_select on_deselect lst =
  filter_dropdown_menu
    label
    (List.map begin fun x ->
      filter_dropdown_selection x (label_creator x) on_select on_deselect
     end lst)
 
let warnings_dropdown warnings_info file_content_data =
  let filter warning_info_filter_value warning_info =
    (* warning_info_filter_value.warning_type.decl.short_name *)
    (* != warning_info.warning_type.decl.short_name *)
    not (
      String.equal
        warning_info_filter_value.warning_type.decl.short_name
        warning_info.warning_type.decl.short_name
    )
  in
  dropdown_creator
    "warnings"
    Web_utils.warning_name
    begin fun warning ->
      (* remove the filter *)
      Web_utils.remove_file_content_filter
        file_content_data
        (Web_utils.Warning_type_filter warning.warning_type.decl.short_name)
      ;
      Web_utils.eval_file_content_filters file_content_data
    end
    begin fun warning ->
      (* filtering the warnings that are not the same type of the
         unchecked warning *)
      Web_utils.add_file_content_filter
        file_content_data  	    
        (Web_utils.Warning_type_filter warning.warning_type.decl.short_name)
        (filter warning)
      ;
      Web_utils.eval_file_content_filters file_content_data
    end
    warnings_info

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
  searchbox_dom##onkeyup <- Dom_html.handler begin fun _ ->
    let keyword = Js.to_string (searchbox_dom##value) in
    Web_utils.remove_file_content_filter
      file_content_data
      Web_utils.Keyword_filter
    ;
    if keyword != "" then begin
      Web_utils.add_file_content_filter
        file_content_data
        Web_utils.Keyword_filter
        (Web_utils.warning_contains_keyword keyword)
    end;
    Web_utils.eval_file_content_filters file_content_data;
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

let warnings_table_entry warning_info file_content_data =
  let tr =
    tr
      [
        td [warnings_table_col_id warning_info];
        td [warnings_table_col_plugin warning_info];
        td [warnings_table_col_linter warning_info];
        td [warnings_table_col_warning warning_info];
      ]
  in
  let dom_tr = Tyxml_js.To_dom.of_element tr in
  dom_tr##onclick <-Dom_html.handler begin fun _ ->
    Web_utils.focus_file_content
      file_content_data
      (Web_utils.Warning_content warning_info);
    Js._true
  end;
  Web_utils.register_file_content_warning_data
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
        ]
    ]

let warnings_table warnings_info id file_content_data =
  let entry_creator warning_info =
    warnings_table_entry warning_info file_content_data
  in
  let table =
    tablex
      ~a:[

	a_class ["dyntable"];
	
	(* a_id id; *)
	
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

let content
      file_info all_file_warnings_info warning_table_id file_content_data =
  let content_div =
    Tyxml_js.Of_dom.of_element
      (file_content_data.Web_utils.file_content_container)
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
    Web_utils.focus_file_content
      file_content_data
      (Web_utils.File_content);
    Js._true
  end;
  div
    [
      h2 [pcdata file_info.file_name];
      br ();
      content_div;
      br ();
      warnings_filter all_file_warnings_info file_content_data;
      br ();
      all_file_button;
      br ();
      br ();
      warnings_table all_file_warnings_info warning_table_id file_content_data;
    ]
