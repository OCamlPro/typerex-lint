(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Fabrice Le Fessant, INRIA Saclay                   *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* TODO: This entire module would need some hconsing to avoid reallocating
   the entire tree. *)

open Parsetree


module type MapArgument = sig

#if OCAML_VERSION = "4.01.0+ocp1"
  val enter_exception_declaration :
    exception_declaration -> exception_declaration
  val enter_modtype_declaration : modtype_declaration -> modtype_declaration
  val leave_exception_declaration :
    exception_declaration -> exception_declaration
  val leave_modtype_declaration : modtype_declaration -> modtype_declaration
  val enter_core_field_type : core_field_type -> core_field_type
  val leave_core_field_type : core_field_type -> core_field_type
#else
  val enter_case : case -> case
  val leave_case : case -> case
  val enter_module_type_declaration : module_type_declaration -> module_type_declaration
  val leave_module_type_declaration : module_type_declaration -> module_type_declaration
  val enter_module_declaration : module_declaration -> module_declaration
  val leave_module_declaration : module_declaration -> module_declaration
  val enter_constructor_declaration : constructor_declaration -> constructor_declaration
  val leave_constructor_declaration : constructor_declaration -> constructor_declaration
  val enter_label_declaration : label_declaration -> label_declaration
  val leave_label_declaration : label_declaration -> label_declaration
  val enter_module_binding : module_binding -> module_binding
  val leave_module_binding : module_binding -> module_binding
  val enter_value_binding : value_binding -> value_binding
  val leave_value_binding : value_binding -> value_binding
  val enter_open_description : open_description -> open_description
  val leave_open_description : open_description -> open_description

  val enter_type_extension : type_extension -> type_extension
  val leave_type_extension : type_extension -> type_extension

  val enter_extension_constructor : extension_constructor -> extension_constructor
  val leave_extension_constructor : extension_constructor -> extension_constructor

#endif

  val enter_structure : structure -> structure
  val enter_value_description : value_description -> value_description
  val enter_type_declaration : type_declaration -> type_declaration
  val enter_pattern : pattern -> pattern
  val enter_expression : expression -> expression
  val enter_package_type : package_type -> package_type
  val enter_signature : signature -> signature
  val enter_signature_item : signature_item -> signature_item
  val enter_module_type : module_type -> module_type
  val enter_module_expr : module_expr -> module_expr
  val enter_with_constraint : with_constraint -> with_constraint
  val enter_class_expr : class_expr -> class_expr
  val enter_class_signature : class_signature -> class_signature
  val enter_class_description : class_description -> class_description
  val enter_class_type_declaration :
    class_type_declaration -> class_type_declaration
  val enter_class_infos : 'a class_infos -> 'a class_infos
  val enter_class_type : class_type -> class_type
  val enter_core_type : core_type -> core_type
  val enter_class_structure : class_structure -> class_structure
  val enter_class_field : class_field -> class_field
  val enter_structure_item : structure_item -> structure_item
  val leave_structure : structure -> structure
  val leave_value_description : value_description -> value_description
  val leave_type_declaration : type_declaration -> type_declaration
  val leave_pattern : pattern -> pattern
  val leave_expression : expression -> expression
  val leave_package_type : package_type -> package_type
  val leave_signature : signature -> signature
  val leave_signature_item : signature_item -> signature_item
  val leave_module_type : module_type -> module_type
  val leave_module_expr : module_expr -> module_expr
  val leave_with_constraint : with_constraint -> with_constraint
  val leave_class_expr : class_expr -> class_expr
  val leave_class_signature : class_signature -> class_signature
  val leave_class_description : class_description -> class_description
  val leave_class_type_declaration :
    class_type_declaration -> class_type_declaration
  val leave_class_infos : 'a class_infos -> 'a class_infos
  val leave_class_type : class_type -> class_type
  val leave_core_type : core_type -> core_type
  val leave_class_structure : class_structure -> class_structure
  val leave_class_field : class_field -> class_field
  val leave_structure_item : structure_item -> structure_item
  val enter_class_type_field : class_type_field -> class_type_field
  val leave_class_type_field : class_type_field -> class_type_field
end


module MakeMap(Map : MapArgument) : sig
  val map_structure : Parsetree.structure -> Parsetree.structure
  val map_signature : Parsetree.signature -> Parsetree.signature
end = struct

  let may_map f v =
    match v with
        None -> v
      | Some x -> Some (f x)


  open Misc

#if OCAML_VERSION <> "4.01.0+ocp1"
  let map_include_infos map i =
    let {
     pincl_mod;
     pincl_loc;
     pincl_attributes;
    } = i in
     {
     pincl_mod = map pincl_mod;
     pincl_loc;
     pincl_attributes;
    }
#endif

  let rec map_structure str =
    let str = Map.enter_structure str in
    let str = List.map map_structure_item str in
    Map.leave_structure str

  and map_structure_item item =
    let item = Map.enter_structure_item item in
    let str_desc =
      match item.pstr_desc with
      | Pstr_value (rec_flag, list) ->
        Pstr_value (rec_flag, List.map map_value_binding list)


#if OCAML_VERSION = "4.01.0+ocp1"
      | Pstr_eval exp -> Pstr_eval (map_expression exp)
#else
      | Pstr_eval (exp,attributes) -> Pstr_eval (map_expression exp, attributes)
#endif

#if OCAML_VERSION = "4.01.0+ocp1"
      | Pstr_primitive (name, v) ->
        Pstr_primitive (name, map_value_description v)
#else
      | Pstr_primitive v ->
        Pstr_primitive (map_value_description v)
#endif


#if OCAML_VERSION = "4.01.0+ocp1"
      | Pstr_exception (name, decl) ->
        Pstr_exception (name, map_exception_declaration decl)
#else
      | Pstr_exception decl ->
        Pstr_exception (map_exception_declaration decl)
#endif


#if OCAML_VERSION = "4.01.0+ocp1"
      | Pstr_exn_rebind (name, lid) ->
        Pstr_exn_rebind (name, lid)
#endif



#if OCAML_VERSION = "4.01.0+ocp1"
      | Pstr_module (name, mexpr) ->
        Pstr_module (name, map_module_expr mexpr)
#else
      | Pstr_module m ->
        Pstr_module (map_module_binding m)
#endif


#if OCAML_VERSION = "4.01.0+ocp1"
      | Pstr_modtype (name, mtype) ->
        Pstr_modtype (name, map_module_type mtype)
#else
      | Pstr_modtype m ->
        Pstr_modtype (map_module_type_declaration m)
#endif


#if OCAML_VERSION = "4.01.0+ocp1"
      | Pstr_open (ovf, lid) -> Pstr_open (ovf, lid)
#else
      | Pstr_open m -> Pstr_open (map_open_description m)
#endif



#if OCAML_VERSION = "4.01.0+ocp1"
      | Pstr_recmodule list ->
        let list =
          List.map (fun (name, mtype, mexpr) ->
            (name, map_module_type mtype, map_module_expr mexpr)
          ) list
        in
        Pstr_recmodule list
#else
      | Pstr_recmodule list ->
        Pstr_recmodule (List.map map_module_binding list)
#endif

#if OCAML_VERSION = "4.01.0+ocp1"
      | Pstr_type list ->
        Pstr_type (List.map (
          fun (name, decl) ->
            (name, map_type_declaration decl) ) list)
#else
      | Pstr_type list ->
        Pstr_type (List.map map_type_declaration list)
#endif


      | Pstr_class list ->
        let list =
          List.map (fun ci ->
            let ci = Map.enter_class_infos ci in
            let ci_expr = map_class_expr ci.pci_expr in
            (Map.leave_class_infos { ci with pci_expr = ci_expr})
          ) list
        in
        Pstr_class list
      | Pstr_class_type list ->
        let list = List.map (fun ct ->
          let ct = Map.enter_class_infos ct in
          let ci_expr = map_class_type ct.pci_expr in
          Map.leave_class_infos { ct with pci_expr = ci_expr}
        ) list in
        Pstr_class_type list
      | Pstr_include mexpr ->
        Pstr_include (map_include_infos map_module_expr mexpr)

#if OCAML_VERSION <> "4.01.0+ocp1"
    | Pstr_typext t ->
      Pstr_typext (map_type_extension t)
    | Pstr_attribute _
    | Pstr_extension (_, _) -> item.pstr_desc
#endif

   in
    Map.leave_structure_item { item with pstr_desc = str_desc}

  and map_value_description v =
    let v = Map.enter_value_description v in
    let val_desc = map_core_type v.pval_type in
    Map.leave_value_description { v with pval_type = val_desc }

  and map_type_declaration decl =
    let decl = Map.enter_type_declaration decl in
    let typ_cstrs = List.map (fun (ct1, ct2, loc) ->
      (map_core_type ct1,
       map_core_type ct2,
       loc)
    ) decl.ptype_cstrs in
    let typ_kind = match decl.ptype_kind with
        Ptype_abstract -> Ptype_abstract
      | Ptype_variant list ->
        let list = List.map map_constructor_declaration list in
        Ptype_variant list
      | Ptype_record list ->
        let list = List.map map_label_declaration list in
        Ptype_record list
#if OCAML_VERSION <> "4.01.0+ocp1"
      | Ptype_open -> Ptype_open
#endif
    in
    let typ_manifest =
      match decl.ptype_manifest with
        None -> None
      | Some ct -> Some (map_core_type ct)
    in
    Map.leave_type_declaration { decl with
      ptype_cstrs = typ_cstrs;
      ptype_kind = typ_kind;
      ptype_manifest = typ_manifest }

  and map_pattern pat =
    let pat = Map.enter_pattern pat in
    let pat_desc =
      match pat.ppat_desc with
      | Ppat_alias (pat1, text) ->
        let pat1 = map_pattern pat1 in
        Ppat_alias (pat1, text)
      | Ppat_tuple list -> Ppat_tuple (List.map map_pattern list)
#if OCAML_VERSION = "4.01.0+ocp1"
      | Ppat_construct (lid, args, arity) ->
        Ppat_construct (lid, may_map map_pattern args, arity)
#else
      | Ppat_construct (lid, args) ->
        Ppat_construct (lid, may_map map_pattern args)
#endif
      | Ppat_variant (label, pato) ->
        Ppat_variant (label, may_map map_pattern pato)
      | Ppat_record (list, closed) ->
        Ppat_record (List.map (fun (lid, pat) ->
          (lid, map_pattern pat) ) list, closed)
      | Ppat_array list -> Ppat_array (List.map map_pattern list)
      | Ppat_or (p1, p2) ->
        Ppat_or (map_pattern p1, map_pattern p2)
      | Ppat_lazy p -> Ppat_lazy (map_pattern p)
      | Ppat_constraint (p,ct) ->
        Ppat_constraint (map_pattern p, map_core_type  ct)
      | Ppat_constant _
      | Ppat_any
      | Ppat_type _
      | Ppat_unpack _
      | Ppat_var _ -> pat.ppat_desc

#if OCAML_VERSION <> "4.01.0+ocp1"
    | Ppat_interval (_, _) -> pat.ppat_desc
    | Ppat_exception pat -> Ppat_exception (map_pattern pat)
    | Ppat_extension _ -> pat.ppat_desc
#endif

   in
    let pat = if pat.ppat_desc != pat_desc then
        { pat with ppat_desc = pat_desc } else pat in
    Map.leave_pattern pat

  and map_expression exp =
    let exp = Map.enter_expression exp in
    let exp_desc =
      match exp.pexp_desc with
      | Pexp_let (rec_flag, list, exp) ->
        Pexp_let (rec_flag,
                  List.map map_value_binding list,
                  map_expression exp)
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pexp_function (label, expo, cases) ->
        Pexp_function (label, may_map map_expression expo,
                       List.map map_value_binding cases)
#else
      | Pexp_function case_list ->
        Pexp_function (List.map map_case case_list)
      | Pexp_fun (label, expo, pat, exp) ->
        Pexp_fun (label, may_map map_expression expo,
                  map_pattern pat, map_expression exp)
#endif

      | Pexp_apply (exp, list) ->
        Pexp_apply (map_expression exp,
                    List.map (fun (label, exp) ->
                      (label, map_expression exp)
                    ) list )
      | Pexp_match (exp, list) ->
        Pexp_match (
          map_expression exp,
          List.map map_case list
        )
      | Pexp_try (exp, list) ->
        Pexp_try (
          map_expression exp,
          List.map map_case list
        )
      | Pexp_tuple list ->
        Pexp_tuple (List.map map_expression list)

#if OCAML_VERSION = "4.01.0+ocp1"
      | Pexp_construct (lid, args, arity) ->
        Pexp_construct (lid, may_map map_expression args, arity )
#else
      | Pexp_construct (lid, args) ->
        Pexp_construct (lid, may_map map_expression args )
#endif

      | Pexp_variant (label, expo) ->
        let expo =match expo with
            None -> expo
          | Some exp -> Some (map_expression exp)
        in
        Pexp_variant (label, expo)
      | Pexp_record (list, expo) ->
        let list =
          List.map (fun (lid, exp) ->
            (lid, map_expression exp)
          ) list in
        let expo = match expo with
            None -> expo
          | Some exp -> Some (map_expression exp)
        in
        Pexp_record (list, expo)
      | Pexp_field (exp, label) ->
        Pexp_field (map_expression exp, label)
      | Pexp_setfield (exp1, label, exp2) ->
        Pexp_setfield (
          map_expression exp1,
          label,
          map_expression exp2)
      | Pexp_array list ->
        Pexp_array (List.map map_expression list)
      | Pexp_ifthenelse (exp1, exp2, expo) ->
        Pexp_ifthenelse (
          map_expression exp1,
          map_expression exp2,
          match expo with
            None -> expo
          | Some exp -> Some (map_expression exp)
        )
      | Pexp_sequence (exp1, exp2) ->
        Pexp_sequence (
          map_expression exp1,
          map_expression exp2
        )
      | Pexp_while (exp1, exp2) ->
        Pexp_while (
          map_expression exp1,
          map_expression exp2
        )
      | Pexp_for (name, exp1, exp2, dir, exp3) ->
        Pexp_for (
          name,
          map_expression exp1,
          map_expression exp2,
          dir,
          map_expression exp3
        )
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pexp_when (exp1, exp2) ->
        Pexp_when (
          map_expression exp1,
          map_expression exp2
        )
#endif

      | Pexp_send (exp, meth) ->
        Pexp_send (map_expression exp, meth)
      | Pexp_setinstvar (lid, exp) ->
        Pexp_setinstvar (lid, map_expression exp)
      | Pexp_override list ->
        Pexp_override (
          List.map (fun (lid, exp) ->
            (lid, map_expression exp)
          ) list
        )
      | Pexp_letmodule (name, mexpr, exp) ->
        Pexp_letmodule (
          name,
          map_module_expr mexpr,
          map_expression exp
        )
      | Pexp_assert exp -> Pexp_assert (map_expression exp)
      | Pexp_lazy exp -> Pexp_lazy (map_expression exp)
      | Pexp_object cl ->
        Pexp_object (map_class_structure cl)
      | Pexp_pack (mexpr) ->
        Pexp_pack (map_module_expr mexpr)
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pexp_constraint (e, cto1, cto2) ->
        Pexp_constraint (map_expression e,
                         may_map map_core_type cto1,
                         may_map map_core_type cto2)
#else
      | Pexp_constraint (e, ct) ->
        Pexp_constraint (map_expression e,
                         map_core_type ct)
      | Pexp_coerce (e, cto1, ct2) ->
        Pexp_coerce (map_expression e,
                         may_map map_core_type cto1,
                         map_core_type ct2)
#endif

      | Pexp_poly (e, cto) ->
        Pexp_poly (map_expression e, may_map map_core_type cto)
      | Pexp_newtype _
      | Pexp_open _
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pexp_assertfalse
#else
      | Pexp_extension _
#endif

      | Pexp_ident _
      | Pexp_constant _
      | Pexp_new _ -> exp.pexp_desc
    in
    let exp = if exp.pexp_desc != exp_desc then
        { exp with pexp_desc = exp_desc    } else exp in
    Map.leave_expression exp

  and map_package_type pack =
    let pack = Map.enter_package_type pack in
    let pack_fields = List.map (
      fun (s, ct) -> (s, map_core_type ct) ) (snd pack) in
    Map.leave_package_type (fst pack, pack_fields)

  and map_signature sg =
    let sg = Map.enter_signature sg in
    let sg = List.map map_signature_item sg in
    Map.leave_signature sg

  and map_signature_item item =
    let item = Map.enter_signature_item item in
    let sig_desc =
      match item.psig_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
      | Psig_value (name, v) ->
          Psig_value (name, map_value_description v)
#else
        Psig_value v ->
          Psig_value (map_value_description v)
#endif

#if OCAML_VERSION = "4.01.0+ocp1"
      | Psig_type list -> Psig_type (
        List.map (fun (name, decl) ->
          (name, map_type_declaration decl)
        ) list
      )
#else
      | Psig_type list -> Psig_type (List.map map_type_declaration list)
#endif


#if OCAML_VERSION = "4.01.0+ocp1"
      | Psig_exception (name, decl) ->
        Psig_exception (name, map_exception_declaration decl)
#else
      | Psig_exception decl ->
        Psig_exception (map_exception_declaration decl)
#endif



#if OCAML_VERSION = "4.01.0+ocp1"
      | Psig_module (name, mtype) ->
        Psig_module (name, map_module_type mtype)
#else
      | Psig_module m ->
        Psig_module (map_module_declaration m)
#endif


#if OCAML_VERSION = "4.01.0+ocp1"
      | Psig_recmodule list ->
        Psig_recmodule (List.map (
          fun (name, mtype) ->
            (name, map_module_type mtype) ) list)
#else
      | Psig_recmodule list ->
        Psig_recmodule (List.map map_module_declaration list)
#endif


#if OCAML_VERSION = "4.01.0+ocp1"
      | Psig_modtype (name, mdecl) ->
        Psig_modtype (name, map_modtype_declaration mdecl)
#else
      | Psig_modtype mdecl ->
        Psig_modtype (map_module_type_declaration mdecl)
#endif

#if OCAML_VERSION = "4.01.0+ocp1"
      | Psig_open _ -> item.psig_desc
#else
      | Psig_open o -> Psig_open (map_open_description o)
#endif

#if OCAML_VERSION = "4.01.0+ocp1"
      | Psig_include mty -> Psig_include (map_module_type mty)
#else
      | Psig_include mty -> Psig_include (
        map_include_infos map_module_type mty)
#endif


      | Psig_class list -> Psig_class (List.map map_class_description list)
      | Psig_class_type list ->
        Psig_class_type (List.map map_class_type_declaration list)

#if OCAML_VERSION <> "4.01.0+ocp1"
      | Psig_typext t ->
            Psig_typext (map_type_extension t)
      | Psig_attribute _
      | Psig_extension _ -> item.psig_desc
#endif
    in
    Map.leave_signature_item { item with psig_desc = sig_desc }


  and map_class_description cd =
    let cd = Map.enter_class_description cd in
    let ci_expr = map_class_type cd.pci_expr in
    Map.leave_class_description { cd with pci_expr = ci_expr}

  and map_class_type_declaration cd =
    let cd = Map.enter_class_type_declaration cd in
    let ci_expr = map_class_type cd.pci_expr in
    Map.leave_class_type_declaration { cd with pci_expr = ci_expr }

  and map_module_type mty =
    let mty = Map.enter_module_type mty in
    let mty_desc =
      match mty.pmty_desc with
        Pmty_ident lid -> mty.pmty_desc
      | Pmty_signature sg -> Pmty_signature (map_signature sg)
      | Pmty_functor (name, mtype1, mtype2) ->
        Pmty_functor (name,
#if OCAML_VERSION <> "4.01.0+ocp1"
                      may_map
#endif
                      map_module_type mtype1,
                      map_module_type mtype2)
      | Pmty_with (mtype, list) ->
        Pmty_with (map_module_type mtype,
#if OCAML_VERSION = "4.01.0+ocp1"
                   List.map (fun (lid, withc) ->
                     (lid, map_with_constraint withc)
                   ) list)
#else
                   List.map map_with_constraint list)
#endif
      | Pmty_typeof mexpr ->
        Pmty_typeof (map_module_expr mexpr)
#if OCAML_VERSION <> "4.01.0+ocp1"
      | Pmty_alias _
      | Pmty_extension _ -> mty.pmty_desc
#endif
    in
    Map.leave_module_type { mty with pmty_desc = mty_desc}

  and map_with_constraint cstr =
    let cstr = Map.enter_with_constraint cstr in
    let cstr =
      match cstr with
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pwith_type decl -> Pwith_type (map_type_declaration decl)
#else
      | Pwith_type (lid,decl) -> Pwith_type (lid, map_type_declaration decl)
#endif

      | Pwith_typesubst decl -> Pwith_typesubst (map_type_declaration decl)
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pwith_module (lid) -> cstr
#else
      | Pwith_module (lid1, lid2) -> cstr
#endif
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pwith_modsubst (lid) -> cstr
#else
      | Pwith_modsubst (s, lid) -> cstr
#endif
    in
    Map.leave_with_constraint cstr

  and map_module_expr mexpr =
    let mexpr = Map.enter_module_expr mexpr in
    let mod_desc =
      match mexpr.pmod_desc with
        Pmod_ident (lid) -> mexpr.pmod_desc
      | Pmod_structure st -> Pmod_structure (map_structure st)
      | Pmod_functor (name, mtype, mexpr) ->
        Pmod_functor (name,
#if OCAML_VERSION <> "4.01.0+ocp1"
                      may_map
#endif
                      map_module_type mtype,
                      map_module_expr mexpr)
      | Pmod_apply (mexp1, mexp2) ->
        Pmod_apply (map_module_expr mexp1,
                    map_module_expr mexp2)
      | Pmod_constraint (mexpr, mod_type ) ->
        Pmod_constraint (map_module_expr mexpr,
                         map_module_type mod_type)
      | Pmod_unpack exp ->
        Pmod_unpack (map_expression exp)
#if OCAML_VERSION <> "4.01.0+ocp1"
      | Pmod_extension _ -> mexpr.pmod_desc
#endif
    in
    Map.leave_module_expr { mexpr with pmod_desc = mod_desc }

  and map_class_expr cexpr =
    let cexpr = Map.enter_class_expr cexpr in
    let cl_desc =
      match cexpr.pcl_desc with
      | Pcl_constraint (ce, ct) ->
        Pcl_constraint (map_class_expr ce, map_class_type ct)
      | Pcl_structure clstr -> Pcl_structure (map_class_structure clstr)
      | Pcl_fun (label, expo, pat, ce) ->
        Pcl_fun (label, may_map map_expression expo,
                 map_pattern pat, map_class_expr ce)

      | Pcl_apply (cl, args) ->
        Pcl_apply (map_class_expr cl,
                   List.map (fun (label, exp) ->
                     (label, map_expression exp)
                   ) args)
      | Pcl_let (rec_flat, bindings, cl) ->
        Pcl_let (rec_flat, List.map map_value_binding bindings,
                 map_class_expr cl)
      | Pcl_constr (loc, ctl) ->
        Pcl_constr (loc, List.map map_core_type ctl)
#if OCAML_VERSION <> "4.01.0+ocp1"
      | Pcl_extension _ -> cexpr.pcl_desc
#endif
    in
    Map.leave_class_expr { cexpr with pcl_desc = cl_desc }

  and map_class_type ct =
    let ct = Map.enter_class_type ct in
    let cltyp_desc =
      match ct.pcty_desc with
        Pcty_signature csg -> Pcty_signature (map_class_signature csg)
      | Pcty_constr (lid, list) ->
        Pcty_constr (lid, List.map map_core_type list)
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pcty_fun (label, ct, cl) ->
        Pcty_fun (label, map_core_type ct, map_class_type cl)
#else
      | Pcty_arrow (label, ct, cl) ->
        Pcty_arrow (label, map_core_type ct, map_class_type cl)
      | Pcty_extension _ -> ct.pcty_desc
#endif
    in
    Map.leave_class_type { ct with pcty_desc = cltyp_desc }

  and map_class_signature cs =
    let cs = Map.enter_class_signature cs in
    let csig_self = map_core_type cs.pcsig_self in
    let csig_fields = List.map map_class_type_field cs.pcsig_fields in
    Map.leave_class_signature {
#if OCAML_VERSION = "4.01.0+ocp1"
        cs with  (* for pcsig_loc *)
#endif
        pcsig_self = csig_self; pcsig_fields = csig_fields }


  and map_class_type_field ctf =
    let ctf = Map.enter_class_type_field ctf in
    let ctf_desc =
      match ctf.pctf_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pctf_inher ct -> Pctf_inher (map_class_type ct)
#else
      | Pctf_inherit ct -> Pctf_inherit (map_class_type ct)
#endif
      | Pctf_val (s, mut, virt, ct) ->
        Pctf_val (s, mut, virt, map_core_type ct)

#if OCAML_VERSION = "4.01.0+ocp1"
      | Pctf_virt  (s, priv, ct) ->
        Pctf_virt (s, priv, map_core_type ct)
      | Pctf_meth  (s, priv, ct) ->
        Pctf_meth (s, priv, map_core_type ct)
      | Pctf_cstr  (ct1, ct2) ->
        Pctf_cstr (map_core_type ct1, map_core_type ct2)
#else
      | Pctf_method  (s, priv, virt, ct) ->
        Pctf_method (s, priv, virt, map_core_type ct)
      | Pctf_constraint  (ct1, ct2) ->
        Pctf_constraint (map_core_type ct1, map_core_type ct2)
      | Pctf_attribute _
      | Pctf_extension _ -> ctf.pctf_desc
#endif
    in
    Map.leave_class_type_field { ctf with pctf_desc = ctf_desc }

  and map_core_type ct =
    let ct = Map.enter_core_type ct in
    let ctyp_desc =
      match ct.ptyp_desc with
        Ptyp_any
      | Ptyp_var _ -> ct.ptyp_desc
      | Ptyp_arrow (label, ct1, ct2) ->
        Ptyp_arrow (label, map_core_type ct1, map_core_type ct2)
      | Ptyp_tuple list -> Ptyp_tuple (List.map map_core_type list)
      | Ptyp_constr (lid, list) ->
        Ptyp_constr (lid, List.map map_core_type list)
      | Ptyp_alias (ct, s) -> Ptyp_alias (map_core_type ct, s)
      | Ptyp_variant (list, bool, labels) ->
        Ptyp_variant (List.map map_row_field list, bool, labels)
      | Ptyp_poly (list, ct) -> Ptyp_poly (list, map_core_type ct)
      | Ptyp_package pack -> Ptyp_package (map_package_type pack)
#if OCAML_VERSION = "4.01.0+ocp1"
      | Ptyp_object list -> Ptyp_object (List.map map_core_field_type list)
      | Ptyp_class (lid, list, labels) ->
        Ptyp_class (lid, List.map map_core_type list, labels)
#else
      | Ptyp_object (list, closed) ->
        Ptyp_object (List.map (fun (s,attrs,ct) ->
          (s, attrs, map_core_type ct)) list, closed)
      | Ptyp_class (lid, list) ->
        Ptyp_class (lid, List.map map_core_type list)
      | Ptyp_extension _ -> ct.ptyp_desc
#endif
   in
    Map.leave_core_type { ct with ptyp_desc = ctyp_desc }

  and map_class_structure cs =
    let cs = Map.enter_class_structure cs in
#if OCAML_VERSION = "4.01.0+ocp1"
    let cstr_pat = map_pattern cs.pcstr_pat in
#else
    let cstr_pat = map_pattern cs.pcstr_self in
#endif
    let cstr_fields = List.map map_class_field cs.pcstr_fields in
    Map.leave_class_structure {
#if OCAML_VERSION = "4.01.0+ocp1"
      pcstr_pat = cstr_pat;
#else
      pcstr_self = cstr_pat;
#endif
      pcstr_fields = cstr_fields }

  and map_row_field rf =
    match rf with
#if OCAML_VERSION = "4.01.0+ocp1"
    | Rtag (label, bool, list) ->
        Rtag (label, bool, List.map map_core_type list)
#else
    | Rtag (label, attrs, bool, list) ->
        Rtag (label, attrs, bool, List.map map_core_type list)
#endif
    | Rinherit ct -> Rinherit (map_core_type ct)

  and map_class_field cf =
    let cf = Map.enter_class_field cf in
    let cf_desc =
      match cf.pcf_desc with
#if OCAML_VERSION = "4.01.0+ocp1"
      | Pcf_constr (cty, cty') ->
        Pcf_constr (map_core_type cty, map_core_type cty')
      | Pcf_valvirt (name, mut, cty) ->
        Pcf_valvirt (name, mut, map_core_type cty)
      | Pcf_val (name, mut, override, exp) ->
        Pcf_val (name, mut, override, map_expression exp)
      | Pcf_virt (name, mut, cty) ->
        Pcf_virt (name, mut, map_core_type cty)
      | Pcf_meth (name, priv, override, exp) ->
        Pcf_meth (name, priv, override, map_expression exp)
      | Pcf_init exp -> Pcf_init (map_expression exp)
      | Pcf_inher (ovf, cl, super) ->
          Pcf_inher (ovf, map_class_expr cl, super)
#else
      | Pcf_inherit (ovf, cl, super) ->
          Pcf_inherit (ovf, map_class_expr cl, super)
      | Pcf_constraint (cty, cty') ->
        Pcf_constraint (map_core_type cty, map_core_type cty')
      | Pcf_val (name, mut, Cfk_concrete (override, exp)) ->
        Pcf_val (name, mut, Cfk_concrete (override, map_expression exp))
      | Pcf_val (name, mut, Cfk_virtual ct) ->
        Pcf_val (name, mut, Cfk_virtual (map_core_type ct))
      | Pcf_method (name, priv, Cfk_concrete (override, exp)) ->
        Pcf_method (name, priv, Cfk_concrete (override, map_expression exp))
      | Pcf_method (name, priv, Cfk_virtual ct) ->
        Pcf_method (name, priv, Cfk_virtual (map_core_type ct))
      | Pcf_initializer exp -> Pcf_initializer (map_expression exp)
      | Pcf_attribute _
      | Pcf_extension _ -> cf.pcf_desc

#endif
    in
    Map.leave_class_field { cf with pcf_desc = cf_desc }

#if OCAML_VERSION = "4.01.0+ocp1"

  and map_case (pat, exp) =
    (map_pattern pat, map_expression exp)

  and map_value_binding (pat, exp) =
    (map_pattern pat, map_expression exp)

  and map_exception_declaration decl =
    let decl = Map.enter_exception_declaration decl in
    let decl = List.map map_core_type decl in
    Map.leave_exception_declaration decl

  and map_constructor_declaration(name, cts, cto, loc) =
   (name, List.map map_core_type cts, may_map map_core_type cto, loc)

  and map_label_declaration (name, mut, ct, loc) =
   (name, mut, map_core_type ct, loc)

  and map_include_infos map mexpr = map mexpr

  and map_modtype_declaration mdecl =
    let mdecl = Map.enter_modtype_declaration mdecl in
    let mdecl =
      match mdecl with
        Pmodtype_abstract -> Pmodtype_abstract
      | Pmodtype_manifest mtype ->
        Pmodtype_manifest (map_module_type mtype)
    in
    Map.leave_modtype_declaration mdecl

  and map_core_field_type cft =
    let cft = Map.enter_core_field_type cft in
    let field_desc = match cft.pfield_desc with
        Pfield_var -> Pfield_var
      | Pfield (s, ct) -> Pfield (s, map_core_type ct)
    in
    Map.leave_core_field_type { cft with pfield_desc = field_desc }

#else


  and map_case case =
    let  {
     pc_lhs;
     pc_guard;
     pc_rhs;
    } = Map.enter_case case in
    let case = {
     pc_lhs = map_pattern pc_lhs;
     pc_guard = may_map map_expression pc_guard;
     pc_rhs = map_expression pc_rhs;
    } in
    Map.leave_case case

  and map_exception_declaration decl =
    let decl = map_extension_constructor decl in
    decl

  and map_type_extension c =
   let {
     ptyext_path;
     ptyext_params;
     ptyext_constructors;
     ptyext_private;
     ptyext_attributes;
   } = Map.enter_type_extension c in
   let c = {
     ptyext_params = List.map (fun (ct, var) -> (map_core_type ct, var)) ptyext_params;
     ptyext_constructors = List.map map_extension_constructor ptyext_constructors;
     ptyext_path;
     ptyext_private;
     ptyext_attributes;
   } in
   Map.leave_type_extension c

  and map_extension_constructor c =
     let {
     pext_name;
     pext_kind;
     pext_loc;
     pext_attributes;
     } = Map.enter_extension_constructor c in
     let c = {
     pext_name;
       pext_kind = (match pext_kind with
         Pext_decl (ctl, cto) ->
           let ctl = List.map map_core_type ctl in
           let cto = may_map map_core_type cto in
           Pext_decl(ctl, cto)
       | Pext_rebind _ -> pext_kind);
     pext_loc;
     pext_attributes;
     } in
     Map.leave_extension_constructor c

(*
and extension_constructor =
    {
     pext_name: string loc;
     pext_kind : extension_constructor_kind;
     pext_loc : Location.t;
     pext_attributes: attributes; (* C [@id1] [@id2] of ... *)
    }

and extension_constructor_kind =
    Pext_decl of core_type list * core_type option
  | Pext_rebind of Longident.t loc
*)
  and map_value_binding v =
    let {
      pvb_pat;
      pvb_expr;
      pvb_attributes;
      pvb_loc;
    } = Map.enter_value_binding v in
    let v = {
      pvb_pat = map_pattern pvb_pat;
      pvb_expr = map_expression pvb_expr;
      pvb_attributes;
      pvb_loc;
    } in
    Map.leave_value_binding v

  and map_label_declaration l =
    let {
     pld_name;
     pld_mutable;
     pld_type;
     pld_loc;
     pld_attributes;
    } = Map.enter_label_declaration l in
    let l = {
     pld_name;
     pld_mutable;
     pld_type = map_core_type pld_type;
     pld_loc;
     pld_attributes;
    } in
    Map.leave_label_declaration l

  and map_open_description o =
    let {
     popen_lid;
     popen_override;
     popen_loc;
     popen_attributes;
    } = Map.enter_open_description o in
    let o = {
     popen_lid;
     popen_override;
     popen_loc;
     popen_attributes;
    } in
    Map.leave_open_description o

  and map_module_binding m =
    let {
      pmb_name;
      pmb_expr;
      pmb_attributes;
      pmb_loc;
    } = Map.enter_module_binding m in
    let m = {
      pmb_name;
      pmb_expr = map_module_expr pmb_expr;
      pmb_attributes;
      pmb_loc;
    } in
    Map.leave_module_binding m

  and map_module_declaration m =
    let {
     pmd_name;
     pmd_type;
     pmd_attributes;
     pmd_loc;
    } = Map.enter_module_declaration m in
    let m = {
     pmd_name;
     pmd_type = map_module_type pmd_type;
     pmd_attributes;
     pmd_loc;
    } in
    Map.leave_module_declaration m

  and map_module_type_declaration m =
    let {
     pmtd_name;
     pmtd_type;
     pmtd_attributes;
     pmtd_loc;
    } = Map.enter_module_type_declaration m in
    let m = {
     pmtd_name;
     pmtd_type = may_map map_module_type pmtd_type;
     pmtd_attributes;
     pmtd_loc;
    } in
    Map.leave_module_type_declaration m

  and map_constructor_declaration c =
    let {
     pcd_name;
     pcd_args;
     pcd_res;
     pcd_loc;
     pcd_attributes;
    } = Map.enter_constructor_declaration c in
    let c =  {
     pcd_name;
     pcd_args = List.map map_core_type pcd_args;
     pcd_res = may_map map_core_type pcd_res;
     pcd_loc;
     pcd_attributes;
    } in
    Map.leave_constructor_declaration c

#endif
end


module DefaultMapArgument = struct

  let enter_structure t = t
  let enter_value_description t = t
  let enter_type_declaration t = t
  let enter_pattern t = t
  let enter_expression t = t
  let enter_package_type t = t
  let enter_signature t = t
  let enter_signature_item t = t
  let enter_module_type t = t
  let enter_module_expr t = t
  let enter_with_constraint t = t
  let enter_class_expr t = t
  let enter_class_signature t = t
  let enter_class_description t = t
  let enter_class_type_declaration t = t
  let enter_class_infos t = t
  let enter_class_type t = t
  let enter_class_type_field t = t
  let enter_core_type t = t
  let enter_class_structure t = t
  let enter_class_field t = t
  let enter_structure_item t = t


  let leave_structure t = t
  let leave_value_description t = t
  let leave_type_declaration t = t
  let leave_pattern t = t
  let leave_expression t = t
  let leave_package_type t = t
  let leave_signature t = t
  let leave_signature_item t = t
  let leave_module_type t = t
  let leave_module_expr t = t
  let leave_with_constraint t = t
  let leave_class_expr t = t
  let leave_class_signature t = t
  let leave_class_description t = t
  let leave_class_type_declaration t = t
  let leave_class_infos t = t
  let leave_class_type t = t
  let leave_class_type_field t = t
  let leave_core_type t = t
  let leave_class_structure t = t
  let leave_class_field t = t
  let leave_structure_item t = t

#if OCAML_VERSION = "4.01.0+ocp1"
  let enter_exception_declaration t = t
  let leave_exception_declaration t = t
  let leave_core_field_type t = t
  let enter_core_field_type t = t
  let enter_modtype_declaration t = t
  let leave_modtype_declaration t = t
#else
 let leave_open_description t = t
  let enter_open_description t = t

  let leave_value_binding t = t
  let enter_value_binding t = t

  let leave_module_binding t = t
  let enter_module_binding t = t

  let leave_module_declaration t = t
  let enter_module_declaration t = t

  let leave_module_type_declaration t = t
  let enter_module_type_declaration t = t

  let leave_label_declaration t = t
  let enter_label_declaration t = t

  let leave_constructor_declaration t = t
  let enter_constructor_declaration t = t

  let leave_case t = t
  let enter_case t = t

  let enter_type_extension t = t
  let leave_type_extension t = t

  let enter_extension_constructor t = t
  let leave_extension_constructor t = t

#endif

end
