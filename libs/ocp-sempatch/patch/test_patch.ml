open Ppx_patch

let limit_to_f = let open Range_limiter in {
    always_reject with
    test_value_binding = function binding -> binds_id binding "f";
}

let () =
  let patch =
    []
    >> (add_arg_fun "f" "x" |> Range_limiter.limit_range limit_to_f)
    (* >> rename_var ">>" ">>!" *)
    (* >> (make_fun_call "f" (Ast_helper.Exp.constant (Asttypes.Const_int 2)) |> Range_limiter.limit_range limit_to_f) *)
    (* >> rename_var "x" "y" *)
    >> (rename_var "x" "z" |> Range_limiter.limit_range limit_to_f)
  in Patch_engine.register "patch" patch
