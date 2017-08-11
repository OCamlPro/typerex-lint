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

let warnings_dropdown analysis_info filter_system grid =
  let on_select warning =
    Web_filter_system.remove_filter
      filter_system
      (Web_filter_system.Warning_type_filter warning)
    ;
    Web_filter_system.eval_filters
      filter_system
  in
  let on_deselect warning =
    Web_filter_system.add_warning_filter
      filter_system
      (Web_filter_system.Warning_type_filter warning)
    ;
    Web_filter_system.eval_filters filter_system
  in
  let selections =
    List.map begin fun warning_info ->
      Web_components.dropdown_checkbox_selection
        warning_info
        (Web_utils.warning_name warning_info)
        on_select
        on_deselect
    end (Web_utils.warnings_set analysis_info.warnings_info)
  in
  Web_components.dropdown_menu "warnings" selections grid

let files_dropdown analysis_info filter_system grid =
  let files_info =
    analysis_info.warnings_info
    |> List.map (fun {warning_file; _} -> warning_file)
    |> Web_utils.files_set
  in
  let on_select file =
    Web_filter_system.remove_filter
      filter_system
      (Web_filter_system.Warning_file_filter file)
    ;
    Web_filter_system.eval_filters filter_system
  in
  let on_deselect file =
    Web_filter_system.add_warning_filter
      filter_system
      (Web_filter_system.Warning_file_filter file)
    ;
    Web_filter_system.eval_filters filter_system
  in
  let selections =
    List.map begin fun file_info ->
      Web_components.dropdown_checkbox_selection
        file_info
        file_info.file_name
        on_select
        on_deselect
    end files_info
  in
  Web_components.dropdown_menu "files" selections grid

let filter_searchbox filter_system grid =
  Web_components.searchbox
    filter_system
    (fun kwd -> Web_filter_system.Warning_keyword_filter kwd)
    Web_filter_system.value_of_warning_filter
    grid

let warning_div_filter analysis_info filter_system =
  div
    ~a:[
      a_class ["dashboard-filter"; "row"];
    ]
    [
      files_dropdown
        analysis_info
        filter_system
        ["col-md-1"; "row-vertical-center"];
      warnings_dropdown
        analysis_info
        filter_system
        ["col-md-1"; "row-vertical-center"];
      filter_searchbox
        filter_system
        [
          "col-md-2";
          "col-md-offset-8";
          "col-no-padding";
          "row-vertical-center";
        ];
    ]

let warning_div analysis_info navigation_system filter_system warning_info =
  let div_warning =
    Web_components.warning_box warning_info begin fun () ->
      let file_content_data =
        Web_navigation_system.open_file_tab
          navigation_system
          warning_info.warning_file
      in
      Web_file_content_data.focus_file_content
        file_content_data
        (Web_file_content_data.Warning_content warning_info)
    end
  in
  Web_filter_system.register_element filter_system warning_info div_warning;
  div_warning

let warnings_content navigation_system analysis_info =
  let filter_system = Web_filter_system.create () in
  div
    (
      (warning_div_filter analysis_info filter_system) ::
      (br ()) ::
      (List.map
         (warning_div analysis_info navigation_system filter_system)
         analysis_info.warnings_info)
    )

let warnings_content_empty () =
  h3 [pcdata "There are no warnings provided."]

let content navigation_system analysis_info =
  let content =
    if Web_utils.list_is_empty analysis_info.warnings_info then
      warnings_content_empty ()
    else
      warnings_content navigation_system analysis_info
  in
  div
    ~a:[
      a_class ["container"];
    ]
    [content]
