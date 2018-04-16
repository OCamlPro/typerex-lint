open SimpleConfig

type feature_analysis_mode =
  | Fast
  | Slow

let string_of_mode = function
  | Fast -> "fast"
  | Slow -> "slow"

let mode_of_string = function
  | "fast" -> Fast
  | "slow" -> Slow
  | _ -> failwith "string is not a valid analysis mode"

let default_mode =
  Slow

type feature =
  | Feature_if_then
  | Feature_if_then_else
  | Feature_match
  | Feature_match_exception
  | Feature_try_with
  | Feature_while
  | Feature_for

let string_of_feature = function
  | Feature_if_then -> "if ... then ..."
  | Feature_if_then_else -> "if ... then ... else ..."
  | Feature_match -> "match ... with ..."
  | Feature_match_exception -> "match ... with ... exceptions ..."
  | Feature_try_with -> "try ... with ..."
  | Feature_while -> "while ... do ... done"
  | Feature_for -> "for ... do ... done"

type feature_info = {
  mutable locations : Location.t list;
  mutable count : int;
}

type features_table =
    (feature, feature_info) Hashtbl.t

let create_features_table () =
  Hashtbl.create 64

let get_feature tbl feature =
  try
    Hashtbl.find tbl feature
  with
  | Not_found ->
     let default_feature_info = {
       locations = [];
       count = 0;
     }
     in
     Hashtbl.add tbl feature default_feature_info;
     default_feature_info

let slow_register_feature tbl feature loc =
  let feature_info = get_feature tbl feature in
  feature_info.count <- feature_info.count + 1;
  feature_info.locations <- loc :: feature_info.locations

let fast_register_feature tbl feature loc =
  let feature_info = get_feature tbl feature in
  feature_info.count <- feature_info.count + 1

let string_of_features_table tbl =
  Hashtbl.fold begin fun feature info acc ->
    Printf.sprintf "%s\n%s : %d"
      acc
      (string_of_feature feature)
      (List.length info.locations)
  end tbl "--- features table content ---"

module Plugin = LintParsingPlugin.Plugin

module Linter = Plugin.MakeLint(struct
    let name = "Get Features"
    let version = "1"
    let short_name = "get_features"
    let details = "Extract the features of the language"
    let enabled = true
  end)

type warning =
  | Features of features_table

let w_features_analysis = Linter.new_warning
    ~id:1
    ~short_name:"features_analysis"
    ~msg:"$tbl"
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | Features tbl ->
         w_features_analysis, ["tbl", string_of_features_table tbl]
  end)

module Asttypes = LintParsing_Asttypes
module Parsetree = LintParsing_Parsetree
module Parse = LintParsing_Parse
module Ast_iterator = LintParsing_Ast_iterator

let analysis_mode =
  let opt =
    Linter.create_option
      "mode"
      "Mode of the features detection (fast or slow)"
      "Mode of the features detection (fast or slow)"
      SimpleConfig.string_option
      (string_of_mode default_mode)
  in
  mode_of_string (!!opt)

module MakeArg = struct

  open Asttypes
  open Parsetree
  open Longident

  let enter_expression exp =
    match exp.pexp_desc with
    | Pexp_ifthenelse (_, _, None) ->
       Some (Feature_if_then, exp.pexp_loc)
    | Pexp_ifthenelse (_, _, Some _) ->
       Some (Feature_if_then_else, exp.pexp_loc)
    | Pexp_match (_, cases) ->
       if List.exists begin fun case ->
         match case.pc_lhs.ppat_desc with
         | Ppat_exception _ -> true
         | _ -> false
       end cases then
         Some (Feature_match_exception, exp.pexp_loc)
       else
         Some (Feature_match_exception, exp.pexp_loc)
    | Pexp_try _ ->
       Some (Feature_try_with, exp.pexp_loc)
    | _ ->
       None

  let enter_pattern pat =
    match pat.ppat_desc with
    | _ -> None

  let enter_type typ =
    match typ.ptype_kind with
    | _ -> None

  let main source =
    let ic = open_in source in
    Location.input_name := source;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf source;
    let str =
      try
        LintParsing_Parse.implementation lexbuf
      with exn ->
        close_in ic;
        raise exn
    in
    let open Ast_iterator in
    let table = create_features_table () in
    let register =
      match analysis_mode with
      | Slow -> slow_register_feature
      | Fast -> fast_register_feature
    in
    let process_node_result = function
      | Some (feature, loc) ->
         register table feature loc
      | None ->
         ()
    in
    let this_iterator =
      { default_iterator with
	expr =
	  (fun iterator exp ->
	     process_node_result (enter_expression exp);
	     default_iterator.expr iterator exp);
	pat =
	  (fun iterator pat ->
	     process_node_result (enter_pattern pat);
	     default_iterator.pat iterator pat);
        type_declaration =
	  (fun iterator typ ->
	     process_node_result (enter_type typ);
	     default_iterator.type_declaration iterator typ);
      }
    in
    default_iterator.structure this_iterator str;
    Warnings.report_file source (Features table)

  let main = LintParsingPlugin.wrap_syntax_error main
end

module Main = Linter.MakeInputML(MakeArg)
