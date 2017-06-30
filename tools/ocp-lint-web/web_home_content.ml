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

let warnings_table_col_file warning_info =
  p [pcdata warning_info.warning_file.file_name]

let warnings_table_col_plugin warning_info =
  p [pcdata warning_info.warning_linter.linter_plugin.plugin_name]

let warnings_table_col_linter warning_info =
  p [pcdata warning_info.warning_linter.linter_name]

let warnings_table_col_warning warning_info =
  p [pcdata warning_info.warning_type.output]

let warnings_table_entry warning_info =
  let tr =
    tr
      [
        td [warnings_table_col_file warning_info];
        td [warnings_table_col_plugin warning_info];
        td [warnings_table_col_linter warning_info];
        td [warnings_table_col_warning warning_info];
      ]
  in
  (* todo in datatable.ml *)
  (Tyxml_js.To_dom.of_element tr)##onclick <-Dom_html.handler begin fun _ ->
    Web_navigation_system.open_warning_tab
      warning_info
      (Web_warning_content.warning_content warning_info);
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

let warnings_table warnings_info =
  let entry_creator warning_info =
    warnings_table_entry warning_info
  in
  let table =
    tablex
      ~a:[
        (* setAttribute(Js.string "cellspacing", Js.string "0"); *)
        (* setAttribute(Js.string "width", Js.string "100%"); *)
      ]
      ~thead:warnings_table_head
      [
        tbody (List.map entry_creator warnings_info)
      ]
  in
  Web_data_table.set table;
  table

let div_warning_pie values =
  let settings =
    D3pie.default_settings
    |> D3pie.set_data_content values
    |> D3pie.set_data_sort_order D3pie.Sort_by_value_desc
  in
  let div_pie =
    div []
  in
  D3pie.d3pie (Tyxml_js.To_dom.of_element div_pie) settings;
  div_pie

let warnings_pie_group_by_file warnings_info =
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_file.file_name
       end
    |> List.map begin fun (file, warnings) -> {
         D3pie.label = file;
         D3pie.value = List.length warnings;
       } end
  in
  div_warning_pie values

let warnings_pie_group_by_plugin warnings_info =
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name
       end
    |> List.map begin fun (plugin, warnings) -> {
         D3pie.label = plugin;
         D3pie.value = List.length warnings;
       } end
  in
  div_warning_pie values

let warnings_pie_group_by_linter warnings_info =
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name,
         warning_info.warning_linter.linter_name
       end
    |> List.map begin fun ((plugin,linter), warnings) -> {
         D3pie.label = plugin ^ "." ^ linter;
         D3pie.value = List.length warnings;
       } end
  in
  div_warning_pie values

let warnings_pie_group_by_warning warnings_info =
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name,
         warning_info.warning_linter.linter_name,
         warning_info.warning_type.decl.short_name
       end
    |> List.map begin fun ((_,_,warning), warnings) -> {
         D3pie.label = warning;
         D3pie.value = List.length warnings;
       } end
  in
  div_warning_pie values

let warnings_pie_group_by_severity warnings_info =
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_type.decl.severity
       end
    |> List.map begin fun (severity, warnings) -> {
         D3pie.label = string_of_int severity;
         D3pie.value = List.length warnings;
       } end
  in
  div_warning_pie values

let dashboard_head analysis_info =
  let div_stat stat =
    div
      ~a:[
        a_class ["col-md-2"];
      ]
      [pcdata stat]
  in
  let errors_stat =
    (string_of_int (List.length analysis_info.errors_info))
    ^ " errors raised"
  in
  let warnings_stat =
    (string_of_int (List.length analysis_info.warnings_info))
    ^ " warnings raised"
  in
  let files_stat =
    (string_of_int (List.length analysis_info.files_info))
    ^ " files analyzed"
  in
  let plugins_stat =
    (string_of_int (List.length analysis_info.plugins_info))
    ^ " plugins activated"
  in
  let linters_stat =
    (string_of_int (List.length analysis_info.linters_info))
    ^ " linters activated"
  in
  div
    ~a:[
      a_class ["row"]
    ]
    [
      div_stat errors_stat;
      div_stat warnings_stat;
      div_stat files_stat;
      div_stat plugins_stat;
      div_stat linters_stat;
    ]

let content analysis_info =
  div
    [
      dashboard_head analysis_info;
      br ();
      warnings_pie_group_by_file analysis_info.warnings_info;
      br ();
      warnings_pie_group_by_plugin analysis_info.warnings_info;
      br ();
      warnings_pie_group_by_linter analysis_info.warnings_info;
      br ();
      warnings_pie_group_by_warning analysis_info.warnings_info;
      br ();
      warnings_pie_group_by_severity analysis_info.warnings_info;
      br ();
      warnings_table analysis_info.warnings_info;
    ]
