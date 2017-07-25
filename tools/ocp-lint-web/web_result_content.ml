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

let error_div_head error_info =
  h4
    ~a:[
      a_class ["alert-heading"];
    ]
    [pcdata (Printf.sprintf "Error #%d" error_info.error_id)]

let error_div error_info =
  let file_msg =
    a
      ~a:[
        a_class ["alert-link"];
      ]
      [
        pcdata error_info.error_file.file_name;
      ]
  in
  let error_msg =
    let str =
      match error_info.error_type with
      | Lint_db_types.Db_error e ->
         Lint_db_error.to_string e
      | Lint_db_types.Plugin_error e ->
         Lint_plugin_error.to_string e
      | Lint_db_types.Sempatch_error e ->
         e
      | Lint_db_types.Ocplint_error e ->
         e
    in
    pcdata str
  in
  div
    ~a:[
      a_class ["alert"; "alert-danger"];
    ]
    [
      error_div_head error_info;
      file_msg;
      br ();
      error_msg;
    ]



let file_warnings_table_id file_info =
  "file-" ^ (Digest.to_hex file_info.file_hash) ^ "-warnings-table"




type filter_element = { (* todo param type *)
  filter_element_info : Lint_web_analysis_info.warning_info;
  filter_element_dom : Dom_html.element Js.t;
}

type filter_system = { (* todo param type *)
  filter_activated :
    (string, Lint_web_analysis_info.warning_info -> bool) Hashtbl.t;
  filter_elements :
    filter_element list;
}

let filter_system_create warnings_info warning_dom_creator =
  let filter_elements =
    List.map begin fun warning_info ->
      {
        filter_element_info =
          warning_info;
        filter_element_dom =
          Tyxml_js.To_dom.of_element (warning_dom_creator warning_info)
      }
    end warnings_info
  in
  {
    filter_activated = Hashtbl.create 64;
    filter_elements = filter_elements;
  }

let filter_system_dom_contents filter_system =
  List.map begin fun {filter_element_dom; _} ->
    Tyxml_js.Of_dom.of_element filter_element_dom
  end filter_system.filter_elements

let filter_system_register_filter filter_id filter filter_system =
  Hashtbl.add filter_system.filter_activated filter_id filter

let filter_system_remove_filter filter_id filter_system =
  Hashtbl.remove filter_system.filter_activated filter_id

let filter_system_eval_filters filter_system =
  let full_filter filter_element =
    Hashtbl.fold begin fun _ filter acc ->
      acc && filter filter_element.filter_element_info
    end filter_system.filter_activated true
  in
  List.iter begin fun filter_element ->
    if full_filter filter_element then
      Web_utils.dom_element_display filter_element.filter_element_dom
    else
      Web_utils.dom_element_undisplay filter_element.filter_element_dom
  end filter_system.filter_elements

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

let warnings_dropdown warnings_info filter_system =
  let filter_id warning_info = (* todo enum type *)
    "warning-" ^ Web_utils.warning_name warning_info
  in
  let filter warning_info_filter_value warning_info =
    not (Web_utils.warning_equals warning_info_filter_value warning_info)
  in
  dropdown_creator
    "warnings"
    Web_utils.warning_name
    begin fun warning ->
      (* remove the filter *)
      filter_system_remove_filter
        (filter_id warning)
        filter_system;
      filter_system_eval_filters filter_system
    end
    begin fun warning ->
      (* filtering the warnings that are not the same type of the
         unchecked warning *)
      filter_system_register_filter
        (filter_id warning)
        (filter warning)
        filter_system;
      filter_system_eval_filters filter_system
    end
    warnings_info

let files_dropdown files_info filter_system =
  let filter_id file_info =
    "file-" ^ file_info.file_name
  in
  let filter file_info_filter_value warning_info =
    not (Web_utils.file_equals file_info_filter_value warning_info.warning_file)
  in
  dropdown_creator
    "files"
    (fun file_info -> file_info.file_name)
    begin fun file ->
      (* remove the filter *)
      filter_system_remove_filter
        (filter_id file)
        filter_system;
      filter_system_eval_filters filter_system
    end
    begin fun file ->
      (* filtering the files that are not the same type of the
         unchecked file *)
      filter_system_register_filter
        (filter_id file)
        (filter file)
        filter_system;
      filter_system_eval_filters filter_system
    end
    files_info

let filter_searchbox filter_system =
  let searchbox =
    input
      ~a:[
        a_input_type `Text;
        a_class ["form-control"; "filter-searchbox"];
        a_placeholder "Search..."
      ] ()
  in
  let filter_name = "keyword" in
  let searchbox_dom = Tyxml_js.To_dom.of_input searchbox in
  searchbox_dom##onkeyup <- Dom_html.handler begin fun _ ->
    let keyword = Js.to_string (searchbox_dom##value) in
    filter_system_remove_filter
      filter_name
      filter_system;
    if keyword != "" then begin
      filter_system_register_filter
        filter_name
        (Web_utils.warning_contains_keyword keyword)
        filter_system
    end;
    filter_system_eval_filters filter_system;
    Js._true
  end;
  searchbox

let warning_div_filter files_info warnings_info filter_system =
  div
    ~a:[
      a_class ["dashboard-filter"];
    ]
    [
      files_dropdown files_info filter_system;
      div ~a:[a_class ["filter-separator"]] [];
      warnings_dropdown warnings_info filter_system;
      div ~a:[a_class ["filter-separator"]] [];
      filter_searchbox filter_system;
    ]

let warning_div_head warning_info =
  h4
    ~a:[
      a_class ["alert-heading"];
    ]
    [pcdata (Printf.sprintf "Warning #%d" warning_info.warning_id)]

let warning_div_body warning_info =
  let file_msg =
    a
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
      Printf.sprintf "raised by %s.%s"
        warning_info.warning_linter.linter_plugin.plugin_name
        warning_info.warning_linter.linter_name
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
      pcdata "from ";
      file_msg;
      pcdata " ";
      line_msg;
      br ();
      b [pcdata "/!\\  "]; (* todo img *)
      warning_msg;
      br ();
      linter_msg;
    ]

let warning_div all_warnings_info all_errors_info warning_info =
  let file_warnings_info =
    List.filter begin fun warning ->
      Web_utils.file_equals warning.warning_file warning_info.warning_file
    end all_warnings_info
  in
  let file_errors_info =
    List.filter begin fun error ->
      Web_utils.file_equals error.error_file warning_info.warning_file
    end all_errors_info
  in
  let div_warning =
    div
    ~a:[
      a_class ["alert"; "alert-warning"];
    ]
    [
      warning_div_head warning_info;
      warning_div_body warning_info;
    ]
  in
  (Tyxml_js.To_dom.of_element div_warning)##onclick <- Dom_html.handler
  begin fun _ ->
    let file_content_data =
      Web_file_content.open_tab
	warning_info.warning_file
	file_warnings_info
	file_errors_info
    in
    Web_file_content_data.focus_file_content
      file_content_data
      (Web_file_content_data.Warning_content warning_info)
    ;
    Js._true
  end;
  div_warning

let content warnings_info errors_info =
  let uniq_warnings_info =
    warnings_info
    |> List.sort Web_utils.warning_compare
    |> Web_utils.remove_successive_duplicates Web_utils.warning_equals
  in
  let uniq_files_info =
    warnings_info
    |> List.map (fun {warning_file; _} -> warning_file)
    |> List.sort (fun f f' -> String.compare f.file_name f'.file_name)
    |> Web_utils.remove_successive_duplicates
         (fun f f' -> String.equal f.file_name f'.file_name)
  in
  let filter_system =
    filter_system_create warnings_info (warning_div warnings_info errors_info)
  in
  div
    (* (List.map error_div errors_info *)
    (* @ List.map warning_div warnings_info) *)
    (
      (warning_div_filter uniq_files_info uniq_warnings_info filter_system) ::
      (br ()) ::
      (filter_system_dom_contents filter_system)
    )
