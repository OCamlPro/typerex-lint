module SempatchPlugin = Plugin.MakePlugin (struct
    let name = "Semantic patch plugin"
    let short_name = "sempatch"
    let details = "Detect pattern with semantic patch"
  end)

module Lint = SempatchPlugin.MakeLint(struct
    let name = "Lint from semantic patches"
    let short_name = "sempatch-lint"
    let details = "Lint from semantic patches."
  end)

type warnings =
  | List_function_on_singleton of string
  | Useless_if
  | Backwards_if
  | Useless_else
  | Partial_function of string
  | Inlined_function of string
  | Empty_list_test of string
  | Comparison_to_boolean
  | Abstract_apply
  | Module_name_not_snake_case of string
  | Constant_if
  | Constant_match
  | Match_on_constructor
  | Physical_comparison_on_allocated_litteral
  | Discarded_result of string * string
  | List_operation_on_litteral of string
  | Identity_let
  | Identity_sprintf_string
  | Identity_sprintf_ps
  | Module_type_name_not_uppercase of string
  | Sys_break_implicitly_caught
  | Generic_warning of string

let to_string = function
  | List_function_on_singleton f -> Printf.sprintf "%s on singleton" f
  | Useless_if -> "Useless if"
  | Backwards_if -> "Backwards if"
  | Useless_else -> "Useless else"
  | Partial_function f -> Printf.sprintf "Use of partial function %s" f
  | Inlined_function f -> Printf.sprintf "Use %s" f
  | Empty_list_test t -> Printf.sprintf "Use %s []" t
  | Comparison_to_boolean -> "Comparison to boolean"
  | Abstract_apply -> "Application of an anonymous function"
  | Module_name_not_snake_case m ->
    Printf.sprintf "Module name not in snake case: %s" m
  | Constant_if -> "Both branches of this if are identical"
  | Constant_match -> "All branches of this match are identical"
  | Match_on_constructor -> "Match on constant or constructor"
  | Physical_comparison_on_allocated_litteral -> "Use structural comparison"
  | Discarded_result (used, recommended) ->
    Printf.sprintf "Result of %s discarded, use %s instead" used recommended
  | List_operation_on_litteral f ->
    Printf.sprintf "List operation on litteral: %s" f
  | Identity_let -> "Useless let binding"
  | Identity_sprintf_string -> "Useless sprintf"
  | Identity_sprintf_ps -> "Useless sprintf"
  | Module_type_name_not_uppercase m ->
    Printf.sprintf "Module type name not in uppercase: %s" m
  | Sys_break_implicitly_caught -> "Sys.Break is implicitly caught"
  | Generic_warning patch_name-> patch_name

module Warnings = Lint.MakeWarnings (struct
    type t = warnings

    let report loc w =
      match w with
      | List_function_on_singleton f ->
        Lint.new_warning loc 1 [Warning.kind_code]
          ~short_name:"1"
          ~msg:(to_string w)
          ~args:[]
      | Useless_if ->
        Lint.new_warning loc 2 [Warning.kind_code]
          ~short_name:"2"
          ~msg:(to_string w)
          ~args:[]
      | Backwards_if ->
        Lint.new_warning loc 3 [Warning.kind_code]
          ~short_name:"3"
          ~msg:(to_string w)
          ~args:[]
      | Useless_else ->
        Lint.new_warning loc 4 [Warning.kind_code]
          ~short_name:"4"
          ~msg:(to_string w)
          ~args:[]
      | Partial_function f ->
        Lint.new_warning loc 5 [Warning.kind_code]
          ~short_name:"5"
          ~msg:(to_string w)
          ~args:[]
      | Inlined_function f ->
        Lint.new_warning loc 6 [Warning.kind_code]
          ~short_name:"6"
          ~msg:(to_string w)
          ~args:[]
      | Empty_list_test cmp ->
        Lint.new_warning loc 7 [Warning.kind_code]
          ~short_name:"7"
          ~msg:(to_string w)
          ~args:[]
      | Comparison_to_boolean ->
        Lint.new_warning loc 8 [Warning.kind_code]
          ~short_name:"8"
          ~msg:(to_string w)
          ~args:[]
      | Abstract_apply ->
        Lint.new_warning loc 9 [Warning.kind_code]
          ~short_name:"9"
          ~msg:(to_string w)
          ~args:[]
      | Module_name_not_snake_case modname ->
        Lint.new_warning loc 9 [Warning.kind_code]
          ~short_name:"9"
          ~msg:(to_string w)
          ~args:[]
      | Constant_if ->
        Lint.new_warning loc 10 [Warning.kind_code]
          ~short_name:"10"
          ~msg:(to_string w)
          ~args:[]
      | Constant_match ->
        Lint.new_warning loc 11 [Warning.kind_code]
          ~short_name:"11"
          ~msg:(to_string w)
          ~args:[]
      | Match_on_constructor ->
        Lint.new_warning loc 12 [Warning.kind_code]
          ~short_name:"12"
          ~msg:(to_string w)
          ~args:[]
      | Physical_comparison_on_allocated_litteral ->
        Lint.new_warning loc 13 [Warning.kind_code]
          ~short_name:"13"
          ~msg:(to_string w)
          ~args:[]
      | Discarded_result (_, _) ->
        Lint.new_warning loc 14 [Warning.kind_code]
          ~short_name:"14"
          ~msg:(to_string w)
          ~args:[]
      | List_operation_on_litteral lit ->
        Lint.new_warning loc 15 [Warning.kind_code]
          ~short_name:"15"
          ~msg:(to_string w)
          ~args:[]
      | Identity_let ->
        Lint.new_warning loc 16 [Warning.kind_code]
          ~short_name:"16"
          ~msg:(to_string w)
          ~args:[]
      | Identity_sprintf_string ->
        Lint.new_warning loc 17 [Warning.kind_code]
          ~short_name:"17"
          ~msg:(to_string w)
          ~args:[]
      | Identity_sprintf_ps ->
        Lint.new_warning loc 18 [Warning.kind_code]
          ~short_name:"18"
          ~msg:(to_string w)
          ~args:[]
      | Module_type_name_not_uppercase modname ->
        Lint.new_warning loc 19 [Warning.kind_code]
          ~short_name:"19"
          ~msg:(to_string w)
          ~args:[]
      | Sys_break_implicitly_caught ->
        Lint.new_warning loc 20 [Warning.kind_code]
          ~short_name:"20"
          ~msg:(to_string w)
          ~args:[]
      | Generic_warning _ ->
        Lint.new_warning loc 21 [Warning.kind_code]
          ~short_name:"21"
          ~msg:(to_string w)
          ~args:[]
  end)
(* TO REMOVE *)
module Sempatch = struct
  type t = string list
  let from_channel ic = []
  let get_matches_from_patches t expr = []

end
(* *************************** *)

let lint ast =
  let ic = open_in "sempatch.md" in
  let patches = Sempatch.from_channel ic in
  let matches = Sempatch.get_matches_from_patches patches ast in
  List.iter (fun (patch_name, (env, loc)) ->
       begin match patch_name with
         | "List function on singleton String.concat" ->
           Warnings.report loc (List_function_on_singleton "String.concat")
         | "List function on singleton List.map" ->
           Warnings.report loc (List_function_on_singleton "List.map")
         | "List function on singleton List.fold_left" ->
           Warnings.report loc (List_function_on_singleton "List.fold_left")
         | "List function on singleton List.fold_right" ->
           Warnings.report loc (List_function_on_singleton "List.fold_right")
         | "Useless if" ->
           Warnings.report loc Useless_if
         | "Backwards if" ->
           Warnings.report loc Backwards_if
         | "Useless else" ->
           Warnings.report loc Useless_else
         | "Inlined function Str.first_chars" ->
           Warnings.report loc (Inlined_function "Str.first_chars")
         | "Inlined function Str.string_after" ->
           Warnings.report loc (Inlined_function "Str.string_after")
         | "Inlined function incr" ->
           Warnings.report loc (Inlined_function "incr")
         | "Inlined function decr" ->
           Warnings.report loc (Inlined_function "decr")
         | "Empty list test <>" ->
           Warnings.report loc (Empty_list_test "<>")
         | "Empty list test =" ->
           Warnings.report loc (Empty_list_test "=")
         | "Comparison to boolean = true"  | "Comparison to boolean == true"
         | "Comparison to boolean = false" | "Comparison to boolean == false"
         | "Comparison to boolean <> true" | "Comparison to boolean != false" ->
           Warnings.report loc Comparison_to_boolean
         | "Constant if" ->
           Warnings.report loc Constant_if
         | "Constant match" ->
           Warnings.report loc Constant_match
         | "Match on constructor" ->
           Warnings.report loc Match_on_constructor
         | "Physical comparison on allocated litteral" ->
           Warnings.report loc Physical_comparison_on_allocated_litteral
         | "Discarded result" ->
           Warnings.report loc (Discarded_result ("TODO1", "TODO2"))
         | "List operation on litteral" ->
           Warnings.report loc (List_operation_on_litteral "TODO")
         | "Identity let" ->
           Warnings.report loc Identity_let
         | "Identity sprintf string" ->
           Warnings.report loc Identity_sprintf_string
         | "Identity sprintf ps" ->
           Warnings.report loc Identity_sprintf_ps
         | "Module type name not uppercase" ->
           Warnings.report loc (Module_type_name_not_uppercase "TODO")
         | "Sys break implicitly caught" ->
           Warnings.report loc Sys_break_implicitly_caught

         | "Module name not snake case" -> (* ??? *)
           Warnings.report loc (Module_name_not_snake_case "TODO")
         | "Abstract apply" ->          (* ??? *)
           Warnings.report loc Abstract_apply
         | "Partial function" ->        (* ???? *)
           Warnings.report loc (Partial_function "TODO")

         | patch_name ->
           Warnings.report loc (Generic_warning patch_name)

       end) matches


module Main = Lint.MakeInputStructure(struct
    let main = lint
  end)
