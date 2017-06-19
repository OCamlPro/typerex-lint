open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json

let warning_href warning_entry =
    (Lint_web.web_static_gen_file warning_entry.hash)
    ^ ".html#"
    ^ (string_of_int warning_entry.id)
       
let find_plugin_entry warning_entry plugins_entries =
  List.find begin fun plugin_entry ->
    warning_entry.plugin_name = plugin_entry.plugin_entry_plugin_name
    && warning_entry.linter_name = plugin_entry.plugin_entry_linter_name
  end plugins_entries
