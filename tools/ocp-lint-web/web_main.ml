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
open Lint_web_warning
open Lint_web_plugin

let main_page warnings_entries plugins_entries =
  let tabs, contents =
    Web_navigation_system.create
      (Web_home_content.content warnings_entries plugins_entries)
      Web_linter_content.content
  in
  div
    [
      tabs;
      contents;
    ]

let load_main_page warnings_entries plugins_entries =
  let body = main_page warnings_entries plugins_entries in
  Dom.appendChild (Dom_html.document##body) (Tyxml_js.To_dom.of_element body)
		  			   
let onload _ =
  let warnings_entries =
    database_warning_entries_of_json
      (Web_utils.json_from_js_var Lint_web.warnings_database_var)
  in
  let plugins_entries =
    plugins_database_entries_of_json
      (Web_utils.json_from_js_var Lint_web.plugins_database_var)
  in
  load_main_page warnings_entries plugins_entries;
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload;
