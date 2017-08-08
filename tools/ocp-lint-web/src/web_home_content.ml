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

let pie_25_colors = (* should be read-only *)
  (* todo add 5 color or group by 5% (20colors) *)
  (* from http://there4.io/2012/05/02/google-chart-color-list/ *)
  [|
    "#3366CC"; "#DC3912"; "#FF9900"; "#109618"; "#990099";
    "#3B3EAC"; "#0099C6"; "#DD4477"; "#66AA00"; "#B82E2E";
    "#316395"; "#994499"; "#22AA99"; "#AAAA11"; "#6633CC";
    "#E67300"; "#8B0707"; "#329262"; "#5574A6"; "#3B3EAC";
   |]

let grouping_color =
  "#9DE825" (* todo check *)

let stroke_color =
  "#BABABA"

let default_pie_settings =
  default_settings
  |> set_size_canvas_height 200
  |> set_size_canvas_width 298
  |> set_data_sort_order Sort_by_value_desc
  |> set_size_pie_outer_radius (Radius_percentage 100)
  |> set_size_pie_inner_radius (Radius_percentage 50)
  |> set_inner_label_format Format_none
  |> set_outer_label_format Format_none
  |> set_segments_colors pie_25_colors
  |> set_segment_stroke_color stroke_color
  |> set_load_effect Load_effect_none
  |> set_segment_on_click_effect Segment_on_click_effect_none

let div_warning_pie title overflow pie_settings =
  let div_pie = div [] in
  let tooltip =
    span
      ~a:[
	a_class ["pie-tooltip"];
      ]
      [
      ]
  in
  let dom_tooltip = (Tyxml_js.To_dom.of_element tooltip) in
  let on_mouseover_segment (arg : d3pie_callback_argument) =
    let label =
      if arg.data.is_grouped then
        arg.data.label
      else
        overflow arg.data.label
    in
    dom_tooltip##style##color <- (Js.string arg.color);
    dom_tooltip##textContent <- (Js.some (Js.string label))
  in
  let on_mouseout_segment (arg : d3pie_callback_argument) =
    dom_tooltip##textContent <- (Js.null)
  in
  let settings =
    pie_settings
    |> set_on_mouseover_callback on_mouseover_segment
    |> set_on_mouseout_callback on_mouseout_segment
    |> set_small_segment_grouping Percentage_grouping 4 "other" grouping_color
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
      tooltip;
    ]

let pie_value label count =
  {
    label = label;
    value = count;
    caption = label;
  }

let warnings_pie_group_by_file analysis_info =
  let files_warnings_info =
    Lint_utils.group_by begin fun warning_info ->
      warning_info.warning_file
    end analysis_info.warnings_info
  in
  let on_click_segment (arg : d3pie_callback_argument) =
    if not arg.data.is_grouped then begin
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
    end
  in
  let values =
    List.map begin fun (file, warnings) ->
      pie_value file.file_name (List.length warnings)
    end files_warnings_info
  in
  let settings =
    default_pie_settings
    |> set_on_click_callback on_click_segment
    |> set_data_content values
    |> set_small_segment_grouping Percentage_grouping 4 "other" grouping_color
  in
  div_warning_pie "Files" (Web_utils.filename_overflow 30) settings

let warnings_pie_group_by_plugin analysis_info =
  let on_click_segment arg =
    Js_utils.alert ("plugin : " ^ (string_of_int arg.index))
  in
  let values =
    analysis_info.warnings_info
    |> Lint_utils.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name
       end
    |> List.map begin fun (plugin, warnings) ->
         pie_value plugin (List.length warnings)
       end
  in
  let settings =
    default_pie_settings
    |> set_on_click_callback on_click_segment
    |> set_data_content values
    |> set_small_segment_grouping Percentage_grouping 4 "other" grouping_color
  in
  div_warning_pie "Plugins" (Web_utils.string_overflow 30) settings

let warnings_pie_group_by_linter analysis_info =
  let on_click_segment arg =
    Js_utils.alert ("linter : " ^ (string_of_int arg.index))
  in
  let values =
    analysis_info.warnings_info
    |> Lint_utils.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name,
         warning_info.warning_linter.linter_name
       end
    |> List.map begin fun ((plugin,linter), warnings) ->
         (* todo use linter_name function for label *)
         pie_value (plugin ^ "/" ^ linter) (List.length warnings)
       end
  in
  let settings =
    default_pie_settings
    |> set_on_click_callback on_click_segment
    |> set_data_content values
    |> set_small_segment_grouping Percentage_grouping 4 "other" grouping_color
  in
  div_warning_pie "Linters" (Web_utils.string_overflow 30) settings

let warnings_pie_group_by_warning analysis_info =
  let on_click_segment arg =
    Js_utils.alert ("warning : " ^ (string_of_int arg.index))
  in
  let values =
    analysis_info.warnings_info
    |> Lint_utils.group_by begin fun warning_info ->
         warning_info.warning_linter.linter_plugin.plugin_name,
         warning_info.warning_linter.linter_name,
         warning_info.warning_type.decl.short_name
       end
    |> List.map begin fun ((_,_,warning), warnings) ->
         pie_value warning (List.length warnings)
       end
  in
  let settings =
    default_pie_settings
    |> set_on_click_callback on_click_segment
    |> set_data_content values
    |> set_small_segment_grouping Percentage_grouping 4 "other" grouping_color
  in
  div_warning_pie "Warnings" (Web_utils.string_overflow 30) settings

let warnings_pie_group_by_severity analysis_info =
  let on_click_segment arg =
    Js_utils.alert ("severity : " ^ (string_of_int arg.index))
  in
  let values =
    analysis_info.warnings_info
    |> Lint_utils.group_by begin fun warning_info ->
         warning_info.warning_type.decl.severity
       end
    |> List.map begin fun (severity, warnings) ->
         pie_value (string_of_int severity) (List.length warnings)
       end
  in
  let settings =
    default_pie_settings
    |> set_on_click_callback on_click_segment
    |> set_data_content values
    |> set_small_segment_grouping Percentage_grouping 3 "other" grouping_color
  in
  div_warning_pie "Severities" (Web_utils.string_overflow 30) settings

let dashboard_head analysis_info =
  let div_stat stat msg grid =
    div
      ~a:[
        a_class (grid @ ["tile-stat-container"]);
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
        ("errors raised")
	["col-md-2"; "col-md-offset-1"];
      div_stat
        (string_of_int (List.length analysis_info.warnings_info))
        ("warnings raised")
	["col-md-2"];
      div_stat
        (string_of_int (List.length analysis_info.files_info))
        ("files analyzed")
	["col-md-2"];
      div_stat
        (string_of_int (List.length analysis_info.plugins_info))
        ("plugins activated")
	["col-md-2"];
      div_stat
        (string_of_int (List.length analysis_info.linters_info))
        ("linters activated")
	["col-md-2"];
    ]

let dashboard_content analysis_info =
  let pie_container div_pie grid =
    div
      ~a:[
	a_class (grid @ ["tile-pie-container"]);
      ]
      [div_pie]
  in
  let warnings_button =
    button
      ~a:[
        a_button_type `Button;
        a_class ["btn"; "btn-warning"; "warnings-button"];
      ]
      [pcdata "See all warnings"]
  in
  let errors_button =
    button
      ~a:[
        a_button_type `Button;
        a_class ["btn"; "btn-danger"; "errors-button"];
      ]
      [pcdata "See all errors"]
  in
  (Tyxml_js.To_dom.of_element warnings_button)##onclick <- Dom_html.handler
  begin fun _ ->
   (* todo open static warnings tab *)
    Js._true
  end;
  (Tyxml_js.To_dom.of_element errors_button)##onclick <- Dom_html.handler
  begin fun _ ->
   (* todo open static errors tab *)
    Js._true
  end;
  div
    ~a:[
      a_class ["dashboard-content"];
    ]
    [
      br ();
      div
        ~a:[
          a_class ["row"];
        ]
        [
          div
            ~a:[
              a_class ["col-md-2";  "col-md-offset-4"; "row-vertical-center"];
            ]
            [
              warnings_button;
            ];
          div
            ~a:[
              a_class ["col-md-2"; "row-vertical-center"];
            ]
            [
              errors_button;
            ];
        ];
      br ();
      br ();
      br ();
      div
	~a:[
	  a_class ["row"];
	]
	[
	  pie_container
	    (warnings_pie_group_by_file analysis_info)
	    ["col-md-4"];
	  pie_container
	    (warnings_pie_group_by_plugin analysis_info)
	    ["col-md-4"];
	  pie_container
	    (warnings_pie_group_by_linter analysis_info)
	    ["col-md-4"];
      ];
      br ();
      div
        ~a:[
          a_class ["row"];
        ]
        [
          pie_container
            (warnings_pie_group_by_warning analysis_info)
            ["col-md-4"; "col-md-offset-2"];
          pie_container
            (warnings_pie_group_by_severity analysis_info)
            ["col-md-4"];
        ]
    ]

let content analysis_info =
  div
    ~a:[
      a_class ["container"];
    ]
    [
      dashboard_head analysis_info;
      br ();
      dashboard_content analysis_info;
    ]
