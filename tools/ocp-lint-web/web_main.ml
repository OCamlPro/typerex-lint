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

let main_page analysis_info =
  let warnings_plugins =
    Lint_web.group_by begin fun warning_info ->
      warning_info.warning_linter.linter_plugin.plugin_name
    end analysis_info.warnings_info
  in
  let tabs, contents =
    Web_navigation_system.create
      (Web_home_content.content analysis_info)
      (Web_plugin_content.content warnings_plugins)
      Web_linter_content.content
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
