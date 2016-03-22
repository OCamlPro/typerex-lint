open Ppx_patch

let rec limit_to_toplevel_expr = let open Ast_filter in let open Parsetree in {
    nothing with
    test_structure_item = (fun f stri ->
      match stri with
      | { pstr_desc = Pstr_eval _ } -> true, all
      | _ -> false, limit_to_toplevel_expr);
}

let rec limit_to_f = let open Ast_filter in {
    nothing with
    test_value_binding = (fun f binding -> if binds_id binding "f" then true, all else false, f);
}

let () =
  let patch =
    []
    >>|
    (filter limit_to_f
    >> add_arg_fun "f" "x"
    >> rename_var "x" "y")
    >>| (filter limit_to_toplevel_expr >> rename_var "x" "z")
    >>| (filter Ast_filter.all >> insert_open "Unix")
  in Patch_engine.register "patch" patch
