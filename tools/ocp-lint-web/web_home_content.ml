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
open D3pie
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

let default_pie_settings =
  default_settings
  |> set_size_canvas_height 200
  |> set_size_canvas_width 300
  |> set_data_sort_order Sort_by_value_desc
  |> set_size_pie_outer_radius (Radius_percentage 100)
  |> set_size_pie_inner_radius (Radius_percentage 50)
  |> set_inner_label_format Format_none
  |> set_outer_label_format Format_none
  |> set_tooltip_caption
  |> set_load_effect Load_effect_none
  |> set_segment_on_click_effect Segment_on_click_effect_none

let div_warning_pie values on_click_segment =
  let div_pie =
    div ~a:[a_class ["tile-pie"]] []
  in
  let settings =
    default_pie_settings
    |> set_callbacks on_click_segment
    |> set_data_content values
  in
  let pie = d3pie (Tyxml_js.To_dom.of_element div_pie) settings in
  (* todo delete *)
  ignore pie;
  (* *)
  div_pie

let pie_value label count =
  {
    label = label;
    value = count;
    caption = label;
  }

let warnings_pie_group_by_file warnings_info =
  let on_click_segment arg =
    Js_utils.alert ("file : " ^ (string_of_int arg.index))
  in
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_file.file_name
       end
    |> List.map begin fun (file, warnings) ->
         pie_value file (List.length warnings)
       end
  in
  div_warning_pie values on_click_segment

let warnings_pie_group_by_plugin warnings_info =
  let on_click_segment arg =
    Js_utils.alert ("plugin : " ^ (string_of_int arg.index))
  in
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name
       end
    |> List.map begin fun (plugin, warnings) ->
         pie_value plugin (List.length warnings)
       end
  in
  div_warning_pie values on_click_segment

let warnings_pie_group_by_linter warnings_info =
  let on_click_segment arg =
    Js_utils.alert ("linter : " ^ (string_of_int arg.index))
  in
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name,
         warning_info.warning_linter.linter_name
       end
    |> List.map begin fun ((plugin,linter), warnings) ->
         pie_value (plugin ^ "." ^ linter) (List.length warnings)
       end
  in
  div_warning_pie values on_click_segment

let warnings_pie_group_by_warning warnings_info =
  let on_click_segment arg =
    Js_utils.alert ("warning : " ^ (string_of_int arg.index))
  in
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name,
         warning_info.warning_linter.linter_name,
         warning_info.warning_type.decl.short_name
       end
    |> List.map begin fun ((_,_,warning), warnings) ->
         pie_value warning (List.length warnings)
       end
  in
  div_warning_pie values on_click_segment

let warnings_pie_group_by_severity warnings_info =
  let on_click_segment arg =
    Js_utils.alert ("severity : " ^ (string_of_int arg.index))
  in
  let values =
    warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_type.decl.severity
       end
    |> List.map begin fun (severity, warnings) ->
         pie_value (string_of_int severity) (List.length warnings)
       end
  in
  div_warning_pie values on_click_segment

let dashboard_head analysis_info =
  let div_stat stat msg =
    div
      ~a:[
        a_class ["col-md-2"; "tile-stats-container"];
      ]
      [
        div
          ~a:[
            a_class ["tile-stats"];
          ]
          [
            span ~a:[a_class ["stat-top"]] [pcdata msg];
            div ~a:[a_class ["stat-value"]] [pcdata stat];
            span ~a:[a_class ["stat-bottom"]] [pcdata msg];
          ]
      ]
  in
  div
    ~a:[
      a_class ["row"; "dashboard-header"]
    ]
    [
      div_stat
        (string_of_int (List.length analysis_info.errors_info))
        ("errors raised");
      div_stat
        (string_of_int (List.length analysis_info.warnings_info))
        ("warnings raised");
      div_stat
        (string_of_int (List.length analysis_info.files_info))
        ("files analyzed");
      div_stat
        (string_of_int (List.length analysis_info.plugins_info))
        ("plugins activated");
      div_stat
        (string_of_int (List.length analysis_info.linters_info))
        ("linters activated");
    ]

let content analysis_info =
  div
    [
      dashboard_head analysis_info;
      br ();
      warnings_pie_group_by_file analysis_info.warnings_info;
      warnings_pie_group_by_plugin analysis_info.warnings_info;
      warnings_pie_group_by_linter analysis_info.warnings_info;
      warnings_pie_group_by_warning analysis_info.warnings_info;
      warnings_pie_group_by_severity analysis_info.warnings_info;
      br ();
      br ();
      warnings_table analysis_info.warnings_info;
    ]
