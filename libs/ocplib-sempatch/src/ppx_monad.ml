open Parsetree
module H = Ast_helper
open Ast_mapper

let bind_op = Longident.Lident ">>="
let map_op = Longident.Lident ">|="

let transform_to_fun self operator expr =
  let expr = self.expr self expr in
  let descr = match expr.pexp_desc with
  | Pexp_let (Asttypes.Nonrecursive, [binding], body) ->
    Pexp_apply (
      H.Exp.ident (Location.mkloc operator expr.pexp_loc),
      [
        "", binding.pvb_expr;
        "", H.Exp.fun_ "" None binding.pvb_pat body;
      ]
    )
  | Pexp_match (expr, cases) ->
    Pexp_apply (
      H.Exp.ident (Location.mkloc operator expr.pexp_loc),
      [
        "", expr;
        "", H.Exp.function_ cases;
      ]
    )
  | d -> d
  in default_mapper.expr self { expr with pexp_desc = descr }

let mapper =
  {
    default_mapper with
    expr = (fun self expr ->
        match expr.pexp_desc with
        | Pexp_extension (id, PStr [{pstr_desc = Pstr_eval (e, _); _}])
          when id.Asttypes.txt = "bind" ->
          transform_to_fun self bind_op e
        | Pexp_extension (id, PStr [{pstr_desc = Pstr_eval (e, _); _}])
          when id.Asttypes.txt = "map" ->
          transform_to_fun self map_op e
        | _ -> default_mapper.expr self expr
      )
  }

let () = Ast_mapper.run_main (fun _ -> mapper)
