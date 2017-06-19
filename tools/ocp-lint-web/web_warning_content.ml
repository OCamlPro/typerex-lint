open Tyxml_js.Html
open Lint_web
open Lint_warning_types
open Lint_warning_json
open Lint_plugin_json

let doc = Dom_html.document
       
let warning_content_code_view_header warning_entry =
  div
    ~a:[
      a_class ["panel-heading"];
    ]
    [
      a
	~a:[
	  a_href (Web_utils.warning_href warning_entry);
	]
	[pcdata warning_entry.file_name]
    ]

let warning_content_code_view_body warning_entry =
  div
    ~a:[
      a_class ["panel-body"];
    ]
    [
      iframe
	~a:[
	  a_src (Web_utils.warning_href warning_entry);
	]
	[]
    ]
    
let warning_content_code_view warning_entry =
  div
    ~a:[
      a_class ["panel"; "panel-default"];
    ]
    [
      warning_content_code_view_header warning_entry;
      warning_content_code_view_body warning_entry;
    ]
  
let warning_content warning_entry plugin_entry =
  let warning_desc =
    "Warning " ^ (string_of_int warning_entry.warning_result.decl.id) ^ " :"
  in
  let linter_desc =
    plugin_entry.plugin_entry_linter_name
    ^ " : "
    ^ plugin_entry.plugin_entry_linter_description
  in
  div
    [
      h3 [pcdata warning_desc];
      h3 [pcdata linter_desc];
      br ();
      warning_content_code_view warning_entry;
    ]

