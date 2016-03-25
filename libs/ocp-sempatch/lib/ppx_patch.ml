open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree

type t = Ast_filter.t * Ast_mapper.mapper list

(** Mapper composition *)

let (->>) (filter, patches) mapper = filter, mapper::patches

let (>>) p1 p2 = p2 :: p1

let filter f = (f, [])

let filter_simple f = (Ast_filter.Test f, [])

(** {2 Patches definition} *)

let rename_var ?(rename_def=true) old_name new_name = {
  default_mapper with
  expr =
    begin
      fun mapper expr ->
        match expr with
        | { pexp_desc = Pexp_ident desc; }
          when Ast_filter.txt_is desc (Longident.Lident old_name) ->
          { expr with
            pexp_desc = Pexp_ident { desc with txt = Longident.Lident new_name };
          }
        | p -> default_mapper.expr mapper p
    end;
  pat =
    if rename_def then
      begin
        fun mapper pat ->
          match pat.ppat_desc with
          | Ppat_var { txt = id; loc; } when id = old_name ->
            { pat with
              ppat_desc = Ppat_var { txt = new_name; loc }
            }
          | p -> default_mapper.pat mapper pat
      end
    else default_mapper.pat
}

let add_arg_fun fname arg_name = {
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

let make_fun_call var_name default_arg = {
  default_mapper with
  expr = fun mapper expr ->
    match expr with
    | { pexp_desc = Pexp_ident i; pexp_attributes = attrs } when Ast_filter.txt_is i (Longident.Lident var_name) ->
      { expr with
        pexp_desc = Pexp_apply (Exp.ident (Location.mkloc (Longident.Lident var_name) expr.pexp_loc), [ "", default_arg ]);
      }
    | e -> default_mapper.expr mapper e
}

let insert_at_structure_toplevel (elt_gen : ?loc:Location.t -> unit -> structure_item) = {
  default_mapper with
  structure = (fun _ s ->
      match s with
      | [] -> elt_gen () :: s
      | hd::s' -> elt_gen ~loc:hd.pstr_loc () :: s
    );
}

let insert_open module_name = insert_at_structure_toplevel
    (fun ?loc () ->
       match loc with
       | Some l -> Str.open_ ~loc:l (Opn.mk ~loc:l ~override:Fresh (Location.mkloc (Longident.Lident module_name) l))
       | None -> Str.open_ (Opn.mk ~override:Fresh (Location.mknoloc (Longident.Lident module_name)))
    )

let raw_change x = x

let flatten patches =
  List.map (fun (f, p) -> List.map (Ast_filter.limit_range f) p) patches |> List.flatten

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
