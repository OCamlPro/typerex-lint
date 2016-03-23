open Ppx_patch
open Ast_helper

let rec limit_to_toplevel_expr = let open Ast_filter in let open Parsetree in {
    nothing with
    test_structure_item = (fun f stri ->
      match stri with
      | { pstr_desc = Pstr_eval _ } -> true, all
      | _ -> false, limit_to_toplevel_expr);
}

let rec limit_to_scope_of var = let open Ast_filter in {
    nothing with
    test_value_binding = (fun mapper binding -> if binds_id binding var then true, all else false, mapper);
}

let not_at_toplevel = let open Ast_filter in {
    all with
    test_structure = (fun f _ -> false, all)
  }

let () =
  let patch =
    []
    >> filter (limit_to_scope_of "test1")
       ->> make_fun_call "f" (Ast_helper.Exp.constant ( Asttypes.Const_int 1))
    >> filter (limit_to_scope_of "test2")
       ->> rename_var "y" "x"
    >> filter (limit_to_scope_of "test3")
       ->> add_arg_fun "f" "x"
       ->> make_fun_call "f" (Ast_helper.Exp.constant ( Asttypes.Const_int 2))
    >> filter (limit_to_scope_of "test4")
       ->> insert_open "List"
  in Patch_engine.register "patch" patch
