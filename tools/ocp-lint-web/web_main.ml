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
      tabs;
      contents;
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
