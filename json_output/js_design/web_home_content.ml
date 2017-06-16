open Tyxml_js.Html
open Lint_warning_types
open Lint_warning_json (****)
       
let warnings_table_col_file warning_entry =
  p [pcdata warning_entry.file_name]

let warnings_table_col_plugin warning_entry =
  p [pcdata warning_entry.plugin_name]

let warnings_table_col_linter warning_entry =
  p [pcdata warning_entry.linter_name]
    
let warnings_table_col_warning warning_entry =
  p [pcdata warning_entry.warning_result.output]

let warnings_table_entry warning_entry =
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
    Web_navigation_system.navigation_open_warning_tab_content warning_entry;
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
    
let warnings_table warnings_entries =
  let table =
    tablex
      ~a:[
	(* setAttribute(Js.string "cellspacing", Js.string "0"); *)
	(* setAttribute(Js.string "width", Js.string "100%"); *)
      ]
      ~thead:warnings_table_head
      [(tbody (List.map warnings_table_entry warnings_entries))]
  in
  Web_data_table.set table;
  table
    
let content warnings_entries =
  div
    [
      warnings_table warnings_entries;
    ]
