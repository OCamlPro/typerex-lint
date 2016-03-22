open Ppx_patch

let () =
  let patch =
    []
    (* >> add_arg_fun "f" "x" *)
    (* >> rename_var ">>" ">>!" *)
    >> make_fun_call "f" (Ast_helper.Exp.constant (Asttypes.Const_int 2))
    >> rename_var "x" "y"
    >> rename_var "y" "z"
  in Patch_engine.register "patch" patch
