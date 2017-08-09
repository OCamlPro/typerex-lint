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
      Web_utils.filter_dropdown_checkbox_selection
        error_info
        (Web_utils.error_type error_info)
        on_select
        on_deselect
    end (Web_utils.errors_set analysis_info.errors_info)
  in
  Web_utils.filter_dropdown_menu "errors" selections grid

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
      Web_utils.filter_dropdown_checkbox_selection
        file_info
        file_info.file_name
        on_select
        on_deselect
    end files_info
  in
  Web_utils.filter_dropdown_menu "files" selections grid

let filter_searchbox filter_system grid =
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
       Web_filter_system.remove_filter
         filter_system
         (Web_filter_system.Error_keyword_filter kwd)
    | None ->
       ()
    end;
    begin match keyword with
    | Some kwd ->
       Web_filter_system.add_error_filter
         filter_system
         (Web_filter_system.Error_keyword_filter kwd)
    | None ->
       ()
    end;
    previous_keyword := keyword;
    Web_filter_system.eval_filters
      filter_system
    ;
    Js._true
  end;
  div
    ~a:[
      a_class grid;
    ]
    [searchbox]

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

let error_div_head error_info =
  h4
    ~a:[
      a_class ["alert-heading"];
    ]
    [pcdata (Printf.sprintf "Error #%d" error_info.error_id)]

let error_div_body error_info =
  let file_msg = (* todo change *)
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

let error_div analysis_info filter_system error_info =
  let file_warnings_info =
    List.filter begin fun warning ->
      Web_utils.file_equals warning.warning_file error_info.error_file
    end analysis_info.warnings_info
  in
  let file_errors_info =
    List.filter begin fun error ->
      Web_utils.file_equals error.error_file error_info.error_file
    end analysis_info.errors_info
  in
  let div_error =
    div
      ~a:[
        a_class ["alert"; "alert-danger"; "row"];
      ]
      [
        error_div_head error_info;
	br ();
        error_div_body error_info;
      ]
  in
  let dom_div_error = Tyxml_js.To_dom.of_element div_error in
  dom_div_error##onclick <- Dom_html.handler begin fun _ ->
    let file_content_data =
      Web_file_content.open_tab
	error_info.error_file
	file_warnings_info
	file_errors_info
    in
    Web_file_content_data.focus_file_content
      file_content_data
      (Web_file_content_data.Error_content error_info)
    ;
    Js._true
  end;
  Web_filter_system.register_element filter_system error_info dom_div_error;
  div_error

let errors_content analysis_info =
  let filter_system = Web_filter_system.create () in
  div
    (
      (error_div_filter analysis_info filter_system) ::
      (br ()) ::
      (List.map
         (error_div analysis_info filter_system)
         analysis_info.errors_info
      )
    )

let errors_content_empty () =
  h3 [pcdata "There are no errors provided in this file"]

let content analysis_info =
  let content =
    if Web_utils.list_is_empty analysis_info.errors_info then
      errors_content_empty ()
    else
      errors_content analysis_info
  in
  div
    ~a:[
      a_class ["container"];
    ]
    [content]
