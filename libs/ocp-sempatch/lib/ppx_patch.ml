open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

type unfiltered_patch =
  | Rename_vars of (bool*string*string) list
  | Add_arg_fun of string*string
  | Make_fun_call of string*Parsetree.expression
  | Insert_at_toplevel of (?loc:Location.t -> unit -> Parsetree.structure_item)

let rename_var ?(rename_def=true) x y = Rename_vars[rename_def, x, y]
let add_arg_fun x y = Add_arg_fun(x, y)
let make_fun_call f a = Make_fun_call(f, a)
let insert_at_toplevel e = Insert_at_toplevel e

type t = Ast_filter.t * unfiltered_patch list

(** Mapper composition *)

let (->>) (filter, patches) mapper = filter, mapper::patches

let (>>) p1 p2 = p2 :: p1

let filter f = (f, [])

let filter_simple f = (Ast_filter.F f, [])

(** {2 Patches definition} *)

let rename_vars_ vars = {
  default_mapper with
  expr =
    begin
      let rename_last oldv cond new_name = let open Longident in match oldv with
        | Lident i when cond i -> Lident new_name
        | Ldot (modules, i) when cond i -> Ldot (modules, new_name)
        | id -> id
      in
      fun mapper expr ->
        let new_expr = match expr with
          | { pexp_desc = Pexp_ident ({ txt = var; _ } as desc); _ } ->
            let new_var = List.fold_left (fun expr (_, oldv, newv) -> rename_last expr ((=) oldv) newv) var vars in
            { expr with
              pexp_desc = Pexp_ident { desc with txt = new_var; };
            }
          | p -> p
        in default_mapper.expr mapper new_expr
    end;
  pat =
    begin
      let rename_pattern_desc oldv cond new_name = match oldv with
        (* TODO: check the other patterns to look at *)
        | Ppat_var v when cond v.txt -> Ppat_var { v with txt = new_name }
        | Ppat_alias (al_pat, v) when cond v.txt -> Ppat_alias (al_pat, { v with txt = new_name; })
        | p -> p
      in
      fun mapper pat ->
        let new_pattern =
          { pat with
            ppat_desc = List.fold_left
                (fun pdesc (rename_def, oldv, newv) ->
                   if rename_def then rename_pattern_desc pdesc ((=) oldv) newv
                   else pdesc
                ) pat.ppat_desc vars;
          }
        in
        default_mapper.pat mapper new_pattern
    end
}

let add_arg_fun_ fname arg_name = {
  default_mapper with
  value_binding =
    fun mapper binding ->
      if Ast_filter.binds_id binding fname then
        let pattern =
          Pat.mk ~loc:binding.pvb_loc (Ppat_var { txt = arg_name; loc = binding.pvb_loc })
        in
        { binding with
          pvb_expr =
            { binding.pvb_expr with
              pexp_desc = Pexp_fun ("", None, pattern, binding.pvb_expr);
            }
        }
      else
        default_mapper.value_binding mapper binding
}

let make_fun_call_ var_name default_arg = {
  default_mapper with
  expr = fun mapper expr ->
    match expr with
    | { pexp_desc = Pexp_ident i; _ } when Ast_filter.txt_is i (Longident.Lident var_name) ->
      { expr with
        pexp_desc = Pexp_apply (Exp.ident (Location.mkloc (Longident.Lident var_name) expr.pexp_loc), [ "", default_arg ]);
      }
    | e -> default_mapper.expr mapper e
}

let insert_at_toplevel_ (elt_gen : ?loc:Location.t -> unit -> structure_item) = {
  default_mapper with
  structure = (fun _ s ->
      match s with
      | [] -> elt_gen () :: s
      | hd::_ -> elt_gen ~loc:hd.pstr_loc () :: s
    );
}

let insert_open module_name = insert_at_toplevel
    (fun ?loc () ->
       match loc with
       | Some l -> Str.open_ ~loc:l (Opn.mk ~loc:l ~override:Fresh (Location.mkloc (Longident.Lident module_name) l))
       | None -> Str.open_ (Opn.mk ~override:Fresh (Location.mknoloc (Longident.Lident module_name)))
    )

let cst x = Ast_helper.Exp.constant x

let symbolic_patch_to_mapper = function
  | Rename_vars renames -> rename_vars_ renames
  | Add_arg_fun (f, argname) -> add_arg_fun_ f argname
  | Make_fun_call (f, arg) -> make_fun_call_ f arg
  | Insert_at_toplevel e -> insert_at_toplevel_ e

let rec simplify_patch = function
  | [] -> []
  | Rename_vars r1 :: Rename_vars r2 :: tl ->
    simplify_patch (Rename_vars (r1 @ r2) :: tl)
  | a::l -> a:: simplify_patch l

let flatten patches =
  let compose f g x = f (g x) in
  List.map (fun (f, p) -> List.map (compose (Ast_filter.limit_range f) symbolic_patch_to_mapper) (simplify_patch p)) patches |> List.flatten

let mkppx patches =
  let patches = flatten patches in
  {
  (* The resulting mapper basically applies all the patches in (reverse) order *)
  (* Overriding the structure and signature fields should be enough if we assume that we always apply ppxes to entire ASTs *)
  default_mapper with
  structure = (
    fun _ structure ->
      List.fold_left (fun iter elt -> elt.structure elt iter) structure (List.rev patches)
  );

  signature = (
    fun _ signature ->
      List.fold_left (fun iter elt -> elt.signature elt iter) signature (List.rev patches)
  );
}

let register name m = Ast_mapper.register name (fun _ -> mkppx m)
let run_main m = Ast_mapper.run_main (fun _ -> mkppx m)
