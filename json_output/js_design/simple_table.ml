open Lint_warning_types
open Lint_warning_json

let doc = Dom_html.document

let summary_entry_file_name_col entry =
  let p = Dom_html.createP doc in
  let a = Dom_html.createA doc in
  let href =
    (Digest.to_hex entry.hash) ^ ".html#" ^ (string_of_int entry.id)
  in
  a##innerHTML <- Js.string entry.file_name;
  if not entry.warning_result.loc.Location.loc_ghost then begin
    a##setAttribute(Js.string "href", Js.string href);
    a##setAttribute(Js.string "target", Js.string "_blank");
  end;
  Dom.appendChild p a;
  p

let summary_entry_plugin_name_col entry =
  let p = Dom_html.createP doc in
  p##innerHTML <- Js.string entry.plugin_name;
  p

let summary_entry_linter_name_col entry =
  let p = Dom_html.createP doc in
  p##innerHTML <- Js.string entry.linter_name;
  p

let summary_entry_warning_output_col entry =
  let p = Dom_html.createP doc in
  let a = Dom_html.createA doc in
  let href = "DOC.html" in
  a##innerHTML <- Js.string entry.warning_result.output;
  a##setAttribute(Js.string "href", Js.string href);
  a##setAttribute(Js.string "target", Js.string "_blank");
  Dom.appendChild p a;
  p

let summary_warning_entry entry =
  let tr = Dom_html.createTr doc in
  let td_file_name = Dom_html.createTd doc in
  let td_plugin_name = Dom_html.createTd doc in
  let td_linter_name = Dom_html.createTd doc in
  let td_warning_output = Dom_html.createTd doc in
  Dom.appendChild td_file_name (summary_entry_file_name_col entry);
  Dom.appendChild tr td_file_name;
  Dom.appendChild td_plugin_name (summary_entry_plugin_name_col entry);
  Dom.appendChild tr td_plugin_name;
  Dom.appendChild td_linter_name (summary_entry_linter_name_col entry);
  Dom.appendChild tr td_linter_name;
  Dom.appendChild td_warning_output (summary_entry_warning_output_col entry);
  Dom.appendChild tr td_warning_output;
  tr

let summary_head_description =
  let thead = Dom_html.createThead doc in
  let tr = Dom_html.createTr doc in
  let th_file_name = Dom_html.createTh doc in
  let th_plugin_name = Dom_html.createTh doc in
  let th_linter_name = Dom_html.createTh doc in
  let th_warning_output = Dom_html.createTh doc in
  th_file_name##innerHTML <- Js.string "File";
  Dom.appendChild tr th_file_name;
  th_plugin_name##innerHTML <- Js.string "Plugin";
  Dom.appendChild tr th_plugin_name;
  th_linter_name##innerHTML <- Js.string "Linter";
  Dom.appendChild tr th_linter_name;
  th_warning_output##innerHTML <- Js.string "Warning";
  Dom.appendChild tr th_warning_output;
  Dom.appendChild thead tr;
  thead
  
let summary_table entries =
  let table = Dom_html.createTable doc in
  let tbody = Dom_html.createTbody doc in
  (*******)
  table##setAttribute(Js.string "id", Js.string "summary-table");
  table##setAttribute(Js.string "class", Js.string "display");
  table##setAttribute(Js.string "cellspacing", Js.string "0");
  table##setAttribute(Js.string "width", Js.string "100%");
  (*******)
  Dom.appendChild table (summary_head_description);
  List.iter begin fun entry ->
    Dom.appendChild tbody (summary_warning_entry entry);
  end entries;
  Dom.appendChild table tbody;
  table

    (****
let create_summary_page db =
  let div = Dom_html.createDiv doc in
  div***)
    
let onload _ =
  let (str_js: Js.js_string Js.t) = Js.Unsafe.variable "json" (* lint_web.var_json_name *) in
  let json = Yojson.Basic.from_string (Js.to_string str_js) in
  let entries = database_warning_entries_of_json json in
  let div = Dom_html.createDiv doc in
  Dom.appendChild div (summary_table entries);
  Dom.appendChild doc##body div;
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload;

