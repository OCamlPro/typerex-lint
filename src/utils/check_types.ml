type kind = Source | Interface | Cmt

type source_check = {
  source_run :
    Configuration.t ->
    Reports.t ->
    string ->
    Reports.t;
  source_info : Info.t;
}

type global_check = {
  global_run :
    Configuration.t ->
    Reports.t ->
    string list ->
    Reports.t;
  global_info : Info.t;
}

let kind_of_string = function
  | Source -> "ml files"
  | Interface -> "mli files"
  | Cmt -> "cmt* files"

let parse_source ~tool_name source =
  Pparse.parse_implementation ~tool_name Format.err_formatter source

let parse_interf ~tool_name interface =
  Pparse.parse_interface ~tool_name Format.err_formatter interface

let check_source mapper file =
  let open Ast_mapper in
  let tool_name = tool_name () in
  if Filename.check_suffix file "ml" then
    let ast = parse_source ~tool_name file in
    ignore (default_mapper.structure mapper ast)
  else ()

let check_interface mapper file =
  let open Ast_mapper in
  let tool_name = tool_name () in
  if Filename.check_suffix file "mli" then
    let ast = parse_interf ~tool_name file in
    ignore (default_mapper.signature mapper ast)
  else ()

let check mapper file =
  check_source mapper file;
  check_interface mapper file
