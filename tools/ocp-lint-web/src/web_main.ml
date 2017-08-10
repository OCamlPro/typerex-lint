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

let header =
  div
    ~a:[
      a_class ["ocp-lint-web-header"]
    ]
    [
      h1 [pcdata "ocp-lint-web"];
    ]

let footer analysis_info =
  let open Unix in
  let time = analysis_info.analysis_date in
  let msg =
    Printf.sprintf "generated the %04d-%02d-%02d at %02d:%02d from %s"
      (1900 + time.tm_year)
      (time.tm_mon + 1)
      time.tm_mday
      time.tm_hour
      time.tm_min
      analysis_info.analysis_root
  in
  div
    ~a:[
      a_class ["ocp-lint-web-footer"]
    ]
    [pcdata msg]

let content_attach_creator analysis_info navigation_system navigation_element =
  let content, attach =
    match navigation_element with
    | Web_navigation_system.HomeElement ->
       Web_home_content.content navigation_system analysis_info,
       Web_navigation_system.No_attached_data
    | Web_navigation_system.WarningsElement ->
       Web_warnings_content.content navigation_system analysis_info,
       Web_navigation_system.No_attached_data
    | Web_navigation_system.ErrorsElement ->
       Web_errors_content.content navigation_system analysis_info,
       Web_navigation_system.No_attached_data
    | Web_navigation_system.FileElement file_info ->
       let file_warnings_info =
         List.filter begin fun warning_info ->
           Web_utils.file_equals file_info warning_info.warning_file
         end analysis_info.warnings_info
       in
       let file_errors_info =
         List.filter begin fun error_info ->
           Web_utils.file_equals file_info error_info.error_file
         end analysis_info.errors_info
       in
       let file_content_data =
         Web_file_content_data.create_file_content_data
           file_info
           file_warnings_info
           file_errors_info
           (div [])
           (Web_file_content.main_content_creator file_info)
       in
       Web_file_content.content file_content_data,
       Web_navigation_system.File_content_attached_data file_content_data
  in
  Tyxml_js.To_dom.of_element content, attach

let main_page analysis_info =
  let navigation_system =
    Web_navigation_system.create (content_attach_creator analysis_info)
  in
  Web_navigation_system.init
    navigation_system;
  let tabs =
    Tyxml_js.Of_dom.of_element
      navigation_system.Web_navigation_system.navigation_dom_tabs
  in
  let contents =
    Tyxml_js.Of_dom.of_element
      navigation_system.Web_navigation_system.navigation_dom_contents
  in
  div
    [
      header;
      br ();
      tabs;
      contents;
      footer analysis_info;
      br ();
    ]

let load_main_page analysis_info =
  let body = main_page analysis_info in
  Dom.appendChild (Dom_html.document##body) (Tyxml_js.To_dom.of_element body)

let onload _ =
  try
    let analysis_info =
      analysis_info_of_json
	(Web_utils.json_from_js_var Lint_web.analysis_info_var)
    in
    load_main_page analysis_info;
    Js._true
  with
  | Web_exception e ->
     process_error e;
     Js._false
  | e ->
     failwith ("uncatched exception " ^ (Printexc.to_string e))

let () =
  Dom_html.window##onload <- Dom_html.handler onload;
