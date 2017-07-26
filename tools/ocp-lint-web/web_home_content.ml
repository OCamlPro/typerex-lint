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

let default_pie_settings =
  default_settings
  |> set_size_canvas_height 200
  |> set_size_canvas_width 298
  |> set_data_sort_order Sort_by_value_desc
  |> set_size_pie_outer_radius (Radius_percentage 100)
  |> set_size_pie_inner_radius (Radius_percentage 50)
  |> set_inner_label_format Format_none
  |> set_outer_label_format Format_none
  |> set_tooltip_caption
  |> set_load_effect Load_effect_none
  |> set_segment_on_click_effect Segment_on_click_effect_none

let div_warning_pie title values on_click_segment =
  let div_pie = div [] in
  let settings =
    default_pie_settings
    |> set_callbacks on_click_segment
    |> set_data_content values
  in
  let pie = d3pie (Tyxml_js.To_dom.of_element div_pie) settings in
  (* todo delete *)
  ignore pie;
  (* *)
  div
    ~a:[
      a_class ["tile-pie"];
    ]
    [
      span
        ~a:[
          a_class ["pie-title"]
        ]
        [pcdata title];
      br ();
      div_pie;
    ]

let pie_value label count =
  {
    label = label;
    value = count;
    caption = label;
  }

let warnings_pie_group_by_file analysis_info =
  let files_warnings_info =
    Lint_web.group_by begin fun warning_info ->
      warning_info.warning_file
    end analysis_info.warnings_info
  in
  let on_click_segment (arg : d3pie_callback_argument) =
    let file_info, file_warnings_info =
      List.find begin fun (file, _) ->
        String.equal file.file_name arg.data.label
      end files_warnings_info
    in
    let file_errors_info =
      List.filter begin fun error ->
        Web_utils.file_equals file_info error.error_file
      end analysis_info.errors_info
    in
    let file_content_data =
      Web_file_content.open_tab file_info file_warnings_info file_errors_info
    in
    begin match Web_file_content_data.active_file_content file_content_data with
    | None ->
       Web_file_content_data.focus_file_content
        file_content_data
        Web_file_content_data.File_content
    | Some _ -> ()
    end
  in
  let values =
    List.map begin fun (file, warnings) ->
      pie_value file.file_name (List.length warnings)
    end files_warnings_info
  in
  div_warning_pie "Files" values on_click_segment

let warnings_pie_group_by_plugin analysis_info =
  let on_click_segment arg =
    Js_utils.alert ("plugin : " ^ (string_of_int arg.index))
  in
  let values =
    analysis_info.warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name
       end
    |> List.map begin fun (plugin, warnings) ->
         pie_value plugin (List.length warnings)
       end
  in
  div_warning_pie "Plugins" values on_click_segment

let warnings_pie_group_by_linter analysis_info =
  let on_click_segment arg =
    Js_utils.alert ("linter : " ^ (string_of_int arg.index))
  in
  let values =
    analysis_info.warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name,
         warning_info.warning_linter.linter_name
       end
    |> List.map begin fun ((plugin,linter), warnings) ->
         pie_value (plugin ^ "." ^ linter) (List.length warnings)
       end
  in
  div_warning_pie "Linters" values on_click_segment

let warnings_pie_group_by_warning analysis_info =
  let on_click_segment arg =
    Js_utils.alert ("warning : " ^ (string_of_int arg.index))
  in
  let values =
    analysis_info.warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name,
         warning_info.warning_linter.linter_name,
         warning_info.warning_type.decl.short_name
       end
    |> List.map begin fun ((_,_,warning), warnings) ->
         pie_value warning (List.length warnings)
       end
  in
  div_warning_pie "Warnings" values on_click_segment

let warnings_pie_group_by_severity analysis_info =
  let on_click_segment arg =
    Js_utils.alert ("severity : " ^ (string_of_int arg.index))
  in
  let values =
    analysis_info.warnings_info
    |> Lint_web.group_by begin fun warning_info ->
         warning_info.warning_type.decl.severity
       end
    |> List.map begin fun (severity, warnings) ->
         pie_value (string_of_int severity) (List.length warnings)
       end
  in
  div_warning_pie "Severities" values on_click_segment

let dashboard_head analysis_info =
  let div_stat stat msg =
    div
      ~a:[
        a_class ["col-md-2"; "tile-stat-container"];
      ]
      [
        div
          ~a:[
            a_class ["tile-stat"];
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

let dashboard_content analysis_info =
  div
    ~a:[
      a_class ["dashboard-content"];
    ]
    [
      warnings_pie_group_by_file analysis_info;
      span [pcdata " "]; (* todo padding *)
      warnings_pie_group_by_plugin analysis_info;
      span [pcdata " "];
      warnings_pie_group_by_linter analysis_info;
      span [pcdata " "];
      warnings_pie_group_by_warning analysis_info;
      span [pcdata " "];
      warnings_pie_group_by_severity analysis_info;
    ]

let content analysis_info =
  div
    [
      dashboard_head analysis_info;
      br ();
      dashboard_content analysis_info;
    ]
