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

let errors_dropdown analysis_info filter_system grid =
  let on_select error =
    Web_filter_system.remove_filter
      filter_system
      (Web_filter_system.Error_type_filter error)
    ;
    Web_filter_system.eval_filters
      filter_system
  in
  let on_deselect error =
    Web_filter_system.add_error_filter
      filter_system
      (Web_filter_system.Error_type_filter error)
    ;
    Web_filter_system.eval_filters filter_system
  in
  let selections =
    List.map begin fun error_info ->
      Web_components.dropdown_checkbox_selection
        error_info
        (Web_utils.error_type error_info)
        on_select
        on_deselect
    end (Web_utils.errors_set analysis_info.errors_info)
  in
  Web_components.dropdown_menu "errors" selections grid

let files_dropdown analysis_info filter_system grid =
  let files_info =
    analysis_info.errors_info
    |> List.map (fun {error_file; _} -> error_file)
    |> Web_utils.files_set
  in
  let on_select file =
    Web_filter_system.remove_filter
      filter_system
      (Web_filter_system.Error_file_filter file)
    ;
    Web_filter_system.eval_filters filter_system
  in
  let on_deselect file =
    Web_filter_system.add_error_filter
      filter_system
      (Web_filter_system.Error_file_filter file)
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
    (fun kwd -> Web_filter_system.Error_keyword_filter kwd)
    Web_filter_system.value_of_error_filter
    grid

let error_div_filter analysis_info filter_system =
  div
    ~a:[
      a_class ["dashboard-filter"; "row"];
    ]
    [
      files_dropdown
        analysis_info
        filter_system
        ["col-md-1"; "row-vertical-center"];
      errors_dropdown
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

let error_div analysis_info navigation_system filter_system error_info =
  let div_error =
    Web_components.error_box error_info begin fun () ->
      let file_content_data =
      Web_navigation_system.open_file_tab
        navigation_system
        error_info.error_file
      in
      Web_file_content_data.focus_file_content
        file_content_data
        (Web_file_content_data.Error_content error_info)
    end
  in
  Web_filter_system.register_element filter_system error_info div_error;
  div_error

let errors_content navigation_system analysis_info =
  let filter_system = Web_filter_system.create () in
  div
    (
      (error_div_filter analysis_info filter_system) ::
      (br ()) ::
      (List.map
         (error_div analysis_info navigation_system filter_system)
         analysis_info.errors_info
      )
    )

let errors_content_empty navigation_system =
  let home_link =
    a
      ~a:[
        a_class ["home-link"];
      ]
      [pcdata "Come back to home page"]
  in
  (Tyxml_js.To_dom.of_element home_link)##onclick <- Dom_html.handler
  begin fun _ ->
    Web_navigation_system.open_home_tab navigation_system;
    Js._true
  end;
  div
    ~a:[
      a_class ["empty-content"];
    ]
    [
      h3 [pcdata "There are no errors provided."];
      br ();
      h4
        [
          home_link;
        ];
      br ();
    ]

let content navigation_system analysis_info =
  let content =
    if Web_utils.list_is_empty analysis_info.errors_info then
      errors_content_empty navigation_system
    else
      errors_content navigation_system analysis_info
  in
  div
    ~a:[
      a_class ["container"];
    ]
    [content]
