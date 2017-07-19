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
  pcdata warning_info.warning_file.file_name

let warnings_table_col_plugin warning_info =
  pcdata warning_info.warning_linter.linter_plugin.plugin_name

let warnings_table_col_linter warning_info =
  pcdata warning_info.warning_linter.linter_name

let warnings_table_col_warning warning_info =
  pcdata warning_info.warning_type.decl.short_name

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

let warnings_table warnings_info id =
  let entry_creator warning_info =
    warnings_table_entry warning_info
  in
  let table =
    tablex
      ~a:[
	a_id id;
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

let warning_li_dropdown_menu
      plugin_name linter_name warning_name warnings_table =
  let tmp = (* todo change *)
    Js.Opt.get
      ((Tyxml_js.To_dom.of_element warnings_table)##lastChild)
      (fun () -> failwith "" (* todo *))
  in
  let warnings_table_entries = (* todo change *)
    Dom.list_of_nodeList (tmp##childNodes)
  in
  let is_checked checkbox =
    Js.to_bool checkbox##checked
  in
  let warn_str =
    Printf.sprintf "%s.%s.%s"
      plugin_name
      linter_name
      warning_name
  in
  let checkbox =
    input
      ~a:[
        a_input_type `Checkbox;
        a_checked ()
      ] ();
  in
  (Tyxml_js.To_dom.of_element checkbox)##onclick <-
    Dom_html.handler begin fun evt ->
      let cb =
        Js.coerce_opt
          (evt##target)
          Dom_html.CoerceTo.input
          (fun _ -> failwith "get checkbox" (* todo web error *))
      in
      let action =
        if is_checked cb then
          (fun e -> Js_utils.log "enable")
        else
          (fun e -> Js_utils.log "disable")
      in
      List.iter begin fun tr ->
        (* action tr; *)
        let tds = Array.of_list (Dom.list_of_nodeList (tr##childNodes)) in
        let pluginval =
          (Js.Opt.get (tds.(1)##firstChild) (fun _ -> failwith ""))##nodeValue
        in
        let plugin =
          Js.to_string (Js.Opt.get (pluginval) (fun _ -> failwith ""))
        in
        let linterval =
          (Js.Opt.get (tds.(2)##firstChild) (fun _ -> failwith ""))##nodeValue
        in
        let linter =
          Js.to_string (Js.Opt.get (linterval) (fun _ -> failwith ""))
        in
        let warningval =
          (Js.Opt.get (tds.(3)##firstChild) (fun _ -> failwith ""))##nodeValue
        in
        let warning =
          Js.to_string (Js.Opt.get (warningval) (fun _ -> failwith ""))
        in
        if
          Printf.sprintf "%s.%s.%s" plugin linter warning
          =
          "plugin_file_system.interface_missing.missing_interface"
        then
          action tr
      end warnings_table_entries;
      Js._true
  end;
  li
    [
      a
        [
          checkbox;
          span ~a:[a_class ["filter-label"]] [pcdata warn_str];
        ]
    ]

let dashboard_filter analysis_info warnings_table =
  let warnings =
    Lint_web.group_by begin fun warning_info ->
      warning_info.warning_linter.linter_plugin.plugin_name,
      warning_info.warning_linter.linter_name,
      warning_info.warning_type.decl.short_name
    end analysis_info.warnings_info
  in
  let dropdown_menu =
    div
      ~a:[
        a_class ["dropdown"];
      ]
      [
        button
          ~a:[
            a_class ["btn"; "btn-default"; "dropdown-toggle"];
            a_button_type `Button;
            a_user_data "toggle" "dropdown";
            (* aria-haspopup true; *)
            (* aria-expanded true *)
          ]
          [pcdata "warnings"];
        span ~a:[a_class ["caret"]] [];
        ul
          ~a:[
            a_class ["dropdown-menu"];
            (* aria-labelledby "dropdownMenu1" *)
          ]
          (List.map begin fun ((pname,lname,wname),_) ->
             warning_li_dropdown_menu pname lname wname warnings_table
           end warnings);
      ]
  in
  div
    ~a:[
      a_class ["dashboard-filter"];
    ]
    [
      dropdown_menu;
    ]

let dashboard_content analysis_info warnings_table =
  div
    ~a:[
      a_class ["dashboard-content"];
    ]
    [
      warnings_pie_group_by_file analysis_info.warnings_info;
      warnings_pie_group_by_plugin analysis_info.warnings_info;
      warnings_pie_group_by_linter analysis_info.warnings_info;
      warnings_pie_group_by_warning analysis_info.warnings_info;
      warnings_pie_group_by_severity analysis_info.warnings_info;
      br ();
      br ();
      warnings_table;
    ]

let content analysis_info warnings_table_id =
  let table = warnings_table analysis_info.warnings_info warnings_table_id in
  div
    [
      dashboard_head analysis_info;
      br ();
      dashboard_filter analysis_info table;
      br ();
      dashboard_content analysis_info table;
    ]
