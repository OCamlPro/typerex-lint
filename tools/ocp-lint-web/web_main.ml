open Tyxml_js.Html
open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json
       
let doc = Dom_html.document

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
  Dom.appendChild (doc##body) (Tyxml_js.To_dom.of_element body)
		  
let json_from_js_var var =
  let (str : Js.js_string Js.t) = Js.Unsafe.variable var in
  Yojson.Basic.from_string (Js.to_string str)
			   
let onload _ =
  let warnings_entries =
    database_warning_entries_of_json
      (json_from_js_var Lint_web.warnings_database_var)
  in
  let plugins_entries =
    plugins_database_entries_of_json
      (json_from_js_var Lint_web.plugins_database_var)
  in
  load_main_page warnings_entries plugins_entries;
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload;
