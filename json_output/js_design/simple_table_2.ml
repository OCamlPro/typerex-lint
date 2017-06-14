open Tyxml_js.Html
open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json
       
let doc = Dom_html.document

let find_plugin_entry warning_entry plugins_entries =
  List.find begin fun plugin_entry ->
    warning_entry.plugin_name = plugin_entry.plugin_entry_plugin_name
    && warning_entry.linter_name = plugin_entry.plugin_entry_linter_name
  end plugins_entries
	    
let main_page warnings_entries plugins_entries =
  let warnings_and_contents =
    List.map begin fun we ->
		   we, 
		   Web_warning_content.warning_content
		     we
		     (find_plugin_entry we plugins_entries)
	     end warnings_entries
  in
  Web_navigation_system.init
    (Web_home_content.content warnings_entries)
    Web_linter_content.content
    warnings_and_contents;
  div
    [
      Web_navigation_system.tabs;
      br ();
      Web_navigation_system.contents;
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
  (***)
  List.iter begin fun pe ->
		  Firebug.console##log (Js.string pe.plugin_entry_linter_name)
	    end plugins_entries;
  (***)
  load_main_page warnings_entries plugins_entries;
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload;
