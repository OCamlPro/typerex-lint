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

let warnings_dropdown warnings_info filter_system grid =
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
      Web_utils.filter_dropdown_checkbox_selection
        warning_info
        (Web_utils.warning_name warning_info)
        on_select
        on_deselect
    end warnings_info
  in
  Web_utils.filter_dropdown_menu "warnings" selections grid

let files_dropdown files_info filter_system grid =
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
         (Web_filter_system.Warning_keyword_filter kwd)
    | None ->
       ()
    end;
    begin match keyword with
    | Some kwd ->
       Web_filter_system.add_warning_filter
         filter_system
         (Web_filter_system.Warning_keyword_filter kwd)
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

let warning_div_filter files_info warnings_info filter_system =
  div
    ~a:[
      a_class ["dashboard-filter"; "row"];
    ]
    [
      files_dropdown
        files_info
        filter_system
        ["col-md-1"; "row-vertical-center"];
      warnings_dropdown
        warnings_info
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

let warning_div_head warning_info =
  h4
    ~a:[
      a_class ["alert-heading"];
    ]
    [pcdata (Printf.sprintf "Warning #%d" warning_info.warning_id)]

let warning_div_body warning_info =
  let file_msg = (* todo change *)
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
        (Web_utils.linter_name warning_info.warning_linter)
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

let warning_div all_warnings_info all_errors_info filter_system warning_info =
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
      a_class ["alert"; "alert-warning"; "row"];
    ]
    [
      warning_div_head warning_info;
      br ();
      warning_div_body warning_info;
    ]
  in
  let dom_div_warning = Tyxml_js.To_dom.of_element div_warning in
  dom_div_warning##onclick <- Dom_html.handler
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
  Web_filter_system.register_element filter_system warning_info dom_div_warning;
  div_warning

let warnings_content analysis_info =
  let uniq_warnings_info =
    analysis_info.warnings_info
    |> List.sort Web_utils.warning_compare
    |> Web_utils.remove_successive_duplicates Web_utils.warning_equals
  in
  let uniq_files_info =
    analysis_info.warnings_info
    |> List.map (fun {warning_file; _} -> warning_file)
    |> List.sort (fun f f' -> String.compare f.file_name f'.file_name)
    |> Web_utils.remove_successive_duplicates
         (fun f f' -> String.equal f.file_name f'.file_name)
  in
  let filter_system = Web_filter_system.create () in
  div
    (
      (warning_div_filter uniq_files_info uniq_warnings_info filter_system) ::
      (br ()) ::
      (List.map
         (warning_div
            analysis_info.warnings_info
            analysis_info.errors_info
            filter_system)
         analysis_info.warnings_info)
    )

let warnings_content_empty () =
  h3 [pcdata "There are no warnings provided in this file"]

let content analysis_info =
  let content =
    if Web_utils.list_is_empty analysis_info.warnings_info then
      warnings_content_empty ()
    else
      warnings_content analysis_info
  in
  div
    ~a:[
      a_class ["container"];
    ]
    [content]
