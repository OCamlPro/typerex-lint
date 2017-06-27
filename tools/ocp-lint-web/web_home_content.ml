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

let warnings_table_col_file warning_entry =
  p [pcdata warning_entry.warning_file_name]

let warnings_table_col_plugin warning_entry =
  p [pcdata warning_entry.warning_plugin_name]

let warnings_table_col_linter warning_entry =
  p [pcdata warning_entry.warning_linter_name]

let warnings_table_col_warning warning_entry =
  p [pcdata warning_entry.warning_result.output]

let warnings_table_entry warning_entry plugin_entry =
  let tr = 
    tr
      [
	td [warnings_table_col_file warning_entry];
	td [warnings_table_col_plugin warning_entry];
	td [warnings_table_col_linter warning_entry];
	td [warnings_table_col_warning warning_entry];
      ]
  in
  (* todo in datatable.ml *)
  (Tyxml_js.To_dom.of_element tr)##onclick <-Dom_html.handler begin fun _ ->
    Web_navigation_system.open_warning_tab
      warning_entry
      (Web_warning_content.warning_content warning_entry plugin_entry);
    Js._true
  end;
  (* *)
  tr

let warnings_table_head =
  thead
    [
      tr
	[
	  th [pcdata "File"];
	  th [pcdata "Plugin"];
	  th [pcdata "Linter"];
	  th [pcdata "Warning"];
	]
    ]

let warnings_table warnings_entries plugins_entries =
  let table =
    tablex
      ~a:[
	(* setAttribute(Js.string "cellspacing", Js.string "0"); *)
	(* setAttribute(Js.string "width", Js.string "100%"); *)
      ]
      ~thead:warnings_table_head
      [(tbody (List.map begin fun warning ->
	          warnings_table_entry
		    warning
		    (Web_utils.find_plugin_entry warning plugins_entries)
	       end warnings_entries))]
  in
  Web_data_table.set table;
  table

let warnings_pie warnings_entries =
  let values =
    warnings_entries
    |> warning_entries_group_by (fun entry -> entry.warning_plugin_name)
    |> List.map begin fun (plugin, entries) ->
         plugin, List.length entries
       end
  in
  let div_pie =
    div []
  in
  D3pie.create_pie (Tyxml_js.To_dom.of_element div_pie) values;
  div_pie
		     
let content warnings_entries plugins_entries =
  let title =
    (string_of_int (List.length warnings_entries)) ^ " warning(s)"
  in
  div
    [
      h3 [pcdata title];
      br ();
      warnings_pie warnings_entries;
      br ();
      warnings_table warnings_entries plugins_entries;
    ]
