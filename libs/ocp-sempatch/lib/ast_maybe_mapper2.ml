open Std_utils
open Parsetree

type 'a t = {
  expr : 'a t -> expression -> expression -> (expression * 'a) option
}

let merge_ast_and_env (env_merger : 'a -> 'a -> 'a) =
  Option.merge_inf (fun (accu_ast, accu_env) (ast, env) ->
      (fun new_env -> (ast :: accu_ast, new_env)) (env_merger accu_env env)
    )


let combine mapper merge default elements patch =
  let results = List.map2 mapper elements patch in
  List.fold_left (merge_ast_and_env merge) (Some ([], default)) results

let traverse_expression merge default self e patch =
  let maybe_desc =
  match e.pexp_desc, patch.pexp_desc with
  | Pexp_ident _, Pexp_ident _
  | Pexp_constant _, Pexp_constant _ -> None
  | Pexp_tuple e1s, Pexp_tuple e2s -> Option.map (fun (trees, env) -> (Pexp_tuple trees, env)) @@ combine (self.expr self) merge default e1s e2s
  (* | Pexp_apply (f1, [lbl1, arg1]), Pexp_apply (f2, [lbl2, arg2]) -> *)
  (*   let mapped_f = self.expr self f1 f2 *)
  (*   and mapped_arg = self.expr self arg1 arg2 |> Option.map (fun x -> [lbl1,x]) in *)
  (*   (match mapped_f, mapped_arg with *)
  (*    | None, None -> None *)
  (*    | Some f, None -> Pexp_apply (f, [lbl1, arg1]) |> Option.some *)
  (*    | None, Some a -> Pexp_apply (f1, a) |> Option.some *)
  (*    | Some f, Some a -> Pexp_apply (f, a) |> Option.some *)
  (*   ) *)
  | _ -> failwith "Non implemented"
  in Option.map (fun (tree, env) -> { e with pexp_desc = tree; }, env) maybe_desc

let map_expr merge default self e patch =
  traverse_expression merge default self e patch

let mk merge default = {
  expr = map_expr merge default
}

