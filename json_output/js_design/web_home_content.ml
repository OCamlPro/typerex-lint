open Tyxml_js.Html
open Lint_warning_types
open Lint_warning_json (****)
       
let warnings_table_col_file warning_entry =
  let href =
    (Digest.to_hex warning_entry.hash)
    ^ ".html#"
    ^ (string_of_int warning_entry.id)
  in
  p
    [
      a
	~a:[
	  a_href href;
	  a_target "_blank"
	]
	[pcdata warning_entry.file_name]
    ]

let warnings_table_col_plugin warning_entry =
  p [pcdata warning_entry.plugin_name]

let warnings_table_col_linter warning_entry =
  p [pcdata warning_entry.linter_name]
    
let warnings_table_col_warning warning_entry =
  let a_open_tab =
     a
       ~a:[
	 a_href "#";
       ]
       [pcdata warning_entry.warning_result.output]
  in
  (Tyxml_js.To_dom.of_element a_open_tab)##onclick <-Dom_html.handler begin
      fun _ ->
      Web_navigation_system.navigation_open_warning_tab_content warning_entry;
      Js._true
  end;
  p
    [
      a_open_tab;
    ]

let warnings_table_entry warning_entry =
  tr
    [
      td [warnings_table_col_file warning_entry];
      td [warnings_table_col_plugin warning_entry];
      td [warnings_table_col_linter warning_entry];
      td [warnings_table_col_warning warning_entry];
    ]

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
  tablex
    ~a:[
      a_id "summary-table";
      a_class ["display"];
      (* setAttribute(Js.string "cellspacing", Js.string "0"); *)
      (* setAttribute(Js.string "width", Js.string "100%"); *)
    ]
    ~thead:warnings_table_head
    [(tbody (List.map warnings_table_entry warnings_entries))]

let content warnings_entries =
  div
    [
      warnings_table warnings_entries;
    ]
