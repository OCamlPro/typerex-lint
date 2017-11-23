(* These tests will simplify the conditions inside the #if macros:
   * Forbid versions < 4.01
   * Forbid 4.03.XXX (not yet supported)
*)
#if OCAML_VERSION < "4.01"
#error "OCaml version < 4.01 not supported"
#endif

(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)


open Asttypes
open Parsetree

module type IteratorArgument = sig

#if OCAML_VERSION < "4.02"
   val enter_exception_declaration : exception_declaration -> unit
   val leave_exception_declaration : exception_declaration -> unit
   val enter_modtype_declaration : modtype_declaration -> unit
   val leave_modtype_declaration : modtype_declaration -> unit
   val enter_core_field_type : core_field_type -> unit
   val leave_core_field_type : core_field_type -> unit
#else
  val enter_type_extension : type_extension -> unit
  val leave_type_extension : type_extension -> unit
  val enter_extension_constructor : extension_constructor -> unit
  val leave_extension_constructor : extension_constructor -> unit
  val enter_module_type_declaration : module_type_declaration -> unit
  val leave_module_type_declaration : module_type_declaration -> unit
#if OCAML_VERSION >= "4.03"
  val enter_types_declaration : Asttypes.rec_flag -> unit
  val leave_types_declaration : Asttypes.rec_flag -> unit
#endif
#endif
    val enter_structure : structure -> unit
    val enter_value_description : value_description -> unit
    val enter_type_declaration : type_declaration -> unit
    val enter_pattern : pattern -> unit
    val enter_expression : expression -> unit
    val enter_package_type : package_type -> unit
    val enter_signature : signature -> unit
    val enter_signature_item : signature_item -> unit
    val enter_module_type : module_type -> unit
    val enter_module_expr : module_expr -> unit
    val enter_with_constraint : with_constraint -> unit
    val enter_class_expr : class_expr -> unit
    val enter_class_signature : class_signature -> unit
    val enter_class_declaration : class_declaration -> unit
    val enter_class_description : class_description -> unit
    val enter_class_type_declaration : class_type_declaration -> unit
    val enter_class_type : class_type -> unit
    val enter_class_type_field : class_type_field -> unit
    val enter_core_type : core_type -> unit
    val enter_class_structure : class_structure -> unit
    val enter_class_field : class_field -> unit
    val enter_structure_item : structure_item -> unit


    val leave_structure : structure -> unit
    val leave_value_description : value_description -> unit
    val leave_type_declaration : type_declaration -> unit
    val leave_pattern : pattern -> unit
    val leave_expression : expression -> unit
    val leave_package_type : package_type -> unit
    val leave_signature : signature -> unit
    val leave_signature_item : signature_item -> unit
    val leave_module_type : module_type -> unit
    val leave_module_expr : module_expr -> unit
    val leave_with_constraint : with_constraint -> unit
    val leave_class_expr : class_expr -> unit
    val leave_class_signature : class_signature -> unit
    val leave_class_declaration : class_declaration -> unit
    val leave_class_description : class_description -> unit
    val leave_class_type_declaration : class_type_declaration -> unit
    val leave_class_type : class_type -> unit
    val leave_class_type_field : class_type_field -> unit
    val leave_core_type : core_type -> unit
    val leave_class_structure : class_structure -> unit
    val leave_class_field : class_field -> unit
    val leave_structure_item : structure_item -> unit

    val enter_bindings : rec_flag -> unit
    val enter_binding : bool -> pattern -> expression -> unit
    val leave_binding : bool -> pattern -> expression -> unit
    val leave_bindings : rec_flag -> unit

      end

module MakeIterator(Iter : IteratorArgument) : sig

    val iter_structure : structure -> unit
    val iter_signature : signature -> unit
    val iter_structure_item : structure_item -> unit
    val iter_signature_item : signature_item -> unit
    val iter_expression : expression -> unit
    val iter_module_type : module_type -> unit
    val iter_pattern : pattern -> unit
    val iter_class_expr : class_expr -> unit

  end = struct

    let may_iter f v =
      match v with
        None -> ()
      | Some x -> f x

#if OCAML_VERSION < "4.02"
    open Asttypes
#endif

    let rec iter_structure str =
      Iter.enter_structure str;
      List.iter iter_structure_item str;
      Iter.leave_structure str


    and iter_structure_item item =
      Iter.enter_structure_item item;
      begin
        match item.pstr_desc with
#if OCAML_VERSION < "4.02"
        | Pstr_eval exp -> iter_expression exp
        | Pstr_primitive (_, v) -> iter_value_description v
        | Pstr_exception (_, decl) -> iter_exception_declaration decl
        | Pstr_exn_rebind (p, _) -> ()
        | Pstr_module (_, mexpr) -> iter_module_expr mexpr
        | Pstr_modtype (_, mtype) -> iter_module_type mtype
        | Pstr_type list ->
          List.iter (fun (_, decl) -> iter_type_declaration decl) list
        | Pstr_recmodule list ->
          List.iter (fun (_, mtype, mexpr) ->
            iter_module_type mtype;
            iter_module_expr mexpr) list
        | Pstr_include mexpr ->
          iter_module_expr mexpr
#elif OCAML_VERSION >= "4.02"
        | Pstr_eval (expr, _attrs) -> iter_expression expr
        | Pstr_module mbind -> iter_module_binding mbind
        | Pstr_primitive v -> iter_value_description v
        | Pstr_typext type_extension -> iter_type_extension type_extension
        | Pstr_exception ext -> iter_extension_constructor ext
        | Pstr_recmodule list -> List.iter iter_module_binding list
        | Pstr_modtype mtd -> iter_module_type_declaration mtd
        | Pstr_extension (ext, _) -> iter_extension ext
        | Pstr_include idecl -> iter_module_expr idecl.pincl_mod
        | Pstr_attribute _attr -> ()
#if OCAML_VERSION < "4.03"
        | Pstr_type list -> List.iter iter_type_declaration list
#else
        | Pstr_type (recflag, list) ->
          Iter.enter_types_declaration recflag;
          List.iter iter_type_declaration list;
          Iter.leave_types_declaration recflag
#endif
#endif
        | Pstr_value (rec_flag, list) -> iter_bindings true rec_flag list
        | Pstr_open _ -> ()
        | Pstr_class list ->
          List.iter (fun ci ->
            Iter.enter_class_declaration ci;
            iter_class_expr ci.pci_expr;
            Iter.leave_class_declaration ci;
          ) list
        | Pstr_class_type list ->
          List.iter (fun ct ->
            Iter.enter_class_type_declaration ct;
            iter_class_type ct.pci_expr;
            Iter.leave_class_type_declaration ct;
          ) list
      end;
      Iter.leave_structure_item item

#if OCAML_VERSION >= "4.02"
    and iter_extension ext =
      begin match snd ext with
      | PStr structure -> iter_structure structure
      | PTyp ct -> iter_core_type ct
      | PPat (pat, expo) ->
        iter_pattern pat;
        may_iter iter_expression expo
#if OCAML_VERSION >= "4.03"
      | PSig sg -> iter_signature sg
#endif
    end

    and iter_case {pc_lhs; pc_guard; pc_rhs} =
      iter_pattern pc_lhs;
      may_iter iter_expression pc_guard;
      iter_expression pc_rhs

    and iter_cases cases =
      List.iter iter_case cases

    and iter_class_field_kind kind =
      begin match kind with
        | Cfk_virtual ct -> iter_core_type ct
        | Cfk_concrete (_flag, exp) -> iter_expression exp
      end

    and iter_module_binding x =
      iter_module_expr x.pmb_expr

    and iter_extension_constructor ext =
      Iter.enter_extension_constructor ext;
      begin match ext.pext_kind with
      | Pext_decl(args, ret) ->
          iter_constructor_arguments args;
          may_iter iter_core_type ret
        | Pext_rebind _ -> ()
      end;
      Iter.leave_extension_constructor ext;

   and iter_constructor_arguments args =
#if OCAML_VERSION >= "4.03"
      match args with
      | Pcstr_record list ->
          List.iter (fun ld -> iter_core_type ld.pld_type) list
      | Pcstr_tuple args ->
#endif
      List.iter iter_core_type args

    and iter_type_extension ptyext =
      Iter.enter_type_extension ptyext;
      List.iter iter_type_parameter ptyext.ptyext_params;
      List.iter iter_extension_constructor ptyext.ptyext_constructors;
      Iter.leave_type_extension ptyext

    and iter_type_parameter (ct, _variance) =
      iter_core_type ct

    and iter_module_type_declaration mtd =
      Iter.enter_module_type_declaration mtd;
      begin
        match mtd.pmtd_type with
        | None -> ()
        | Some mtype -> iter_module_type mtype
      end;
      Iter.leave_module_type_declaration mtd

    and iter_constructor_declaration cd =
      iter_constructor_arguments cd.pcd_args;
      may_iter iter_core_type cd.pcd_res;

#endif
    and iter_value_description v =
      Iter.enter_value_description v;
      iter_core_type v.pval_type;
      Iter.leave_value_description v

    and iter_type_declaration decl =
      Iter.enter_type_declaration decl;
      List.iter (fun (ct1, ct2, _loc) ->
        iter_core_type ct1;
        iter_core_type ct2
      ) decl.ptype_cstrs;
      begin match decl.ptype_kind with
        Ptype_abstract -> ()
#if OCAML_VERSION < "4.02"
      | Ptype_variant list ->
        List.iter (fun (s, cts, cto, loc) ->
          List.iter iter_core_type cts;
          may_iter iter_core_type cto
        ) list
      | Ptype_record list ->
        List.iter (fun (s, mut, ct, loc) ->
          iter_core_type ct
        ) list
#elif OCAML_VERSION >= "4.02"
      | Ptype_variant list -> List.iter iter_constructor_declaration list
      | Ptype_record list ->
        List.iter (fun ld -> iter_core_type ld.pld_type) list
      | Ptype_open -> ()
#else
#endif
      end;
      begin match decl.ptype_manifest with
        None -> ()
      | Some ct -> iter_core_type ct
      end;
      Iter.leave_type_declaration decl

    and iter_pattern pat =
      Iter.enter_pattern pat;
      begin
        match pat.ppat_desc with
          Ppat_any -> ()
        | Ppat_var (_) -> ()
        | Ppat_type _ -> ()
        | Ppat_unpack _ -> ()
        | Ppat_alias (pat1, _) -> iter_pattern pat1
        | Ppat_constant _cst -> ()
        | Ppat_tuple list -> List.iter iter_pattern list
#if OCAML_VERSION < "4.02"
        | Ppat_construct (_, args, _) ->  may_iter iter_pattern args
        | Ppat_constraint (pat, ct) -> iter_pattern pat; iter_core_type ct
#else
        | Ppat_interval (_, _) -> ()
        | Ppat_construct (_, pato) -> may_iter iter_pattern pato
        | Ppat_constraint (pat, ct) ->
          iter_pattern pat;
          iter_core_type ct
        | Ppat_exception pat -> iter_pattern pat
        | Ppat_extension ext -> iter_extension ext
#endif
#if OCAML_VERSION >= "4.04"
        | Ppat_open (_lid, pat) -> iter_pattern pat
#endif
        | Ppat_variant (_label, pato) ->
          may_iter iter_pattern pato
        | Ppat_record (list, _closed) ->
          List.iter (fun (_label, pat) -> iter_pattern pat) list
        | Ppat_array list -> List.iter iter_pattern list
        | Ppat_or (p1, p2) -> iter_pattern p1; iter_pattern p2
        | Ppat_lazy p -> iter_pattern p
      end;
      Iter.leave_pattern pat

    and iter_expression exp =
      Iter.enter_expression exp;
      begin
        match exp.pexp_desc with
          Pexp_ident _path -> ()
#if OCAML_VERSION < "4.02"
        | Pexp_constraint (e, cty1, cty2) ->
          iter_expression e;
          may_iter iter_core_type cty1; may_iter iter_core_type cty2
        | Pexp_function (label, expo, cases) ->
          may_iter iter_expression expo;
          iter_bindings false Nonrecursive cases
        | Pexp_construct (_, expo, _) ->
          may_iter iter_expression expo
        | Pexp_when (exp1, exp2) ->
          iter_expression exp1;
          iter_expression exp2
        | Pexp_assertfalse -> ()
        | Pexp_let (rec_flag, list, exp) ->
          iter_bindings false rec_flag list;
          iter_expression exp
        | Pexp_match (exp, list) ->
          iter_expression exp;
          iter_bindings false Nonrecursive list
        | Pexp_try (exp, list) ->
          iter_expression exp;
          iter_bindings false Nonrecursive list
        | Pexp_letmodule (_, mexpr, exp) ->
          iter_module_expr mexpr;
          iter_expression exp
        | Pexp_pack (mexpr) ->
          iter_module_expr mexpr
#else
        | Pexp_let (rec_flag, vb, exp) ->
          iter_bindings false rec_flag vb;
          iter_expression exp
        | Pexp_function cases -> iter_cases cases
        | Pexp_fun (_label, expo, pat, exp) ->
          may_iter iter_expression expo;
          iter_pattern pat;
          iter_expression exp
        | Pexp_match (exp, cases) ->
          iter_expression exp;
          iter_cases cases
        | Pexp_try (exp, cases) ->
          iter_expression exp;
          iter_cases cases
        | Pexp_construct (_label, expo) -> may_iter iter_expression expo
        | Pexp_constraint (exp, ct) ->
          iter_expression exp;
          iter_core_type ct
        | Pexp_coerce (exp, cto, ct) ->
          iter_expression exp;
          may_iter iter_core_type cto;
          iter_core_type ct
        | Pexp_letmodule (_loc, mexpr, exp) ->
          iter_module_expr mexpr;
          iter_expression exp
        | Pexp_pack mexpr -> iter_module_expr mexpr
        | Pexp_extension ext -> iter_extension ext
#endif
#if OCAML_VERSION >= "4.03"
        | Pexp_unreachable -> ()
#endif
#if OCAML_VERSION >= "4.04"
        | Pexp_letexception (extension_constructor, e) ->
          iter_extension_constructor extension_constructor;
          iter_expression e
#endif
        | Pexp_constant _cst -> ()
        | Pexp_open (_ovflag, _path, e) ->
          iter_expression e
        | Pexp_poly (e, cto) ->
          iter_expression e;
          may_iter iter_core_type cto
        | Pexp_newtype (_s, e) -> iter_expression e
        | Pexp_apply (exp, list) ->
          iter_expression exp;
          List.iter (fun (_label, exp) -> iter_expression exp) list
        | Pexp_tuple list ->
          List.iter iter_expression list
        | Pexp_variant (_label, expo) ->
          may_iter iter_expression expo
        | Pexp_record (list, expo) ->
          List.iter (fun (_, exp) -> iter_expression exp) list;
          may_iter iter_expression expo
        | Pexp_field (exp, _label) ->
          iter_expression exp
        | Pexp_setfield (exp1, _label, exp2) ->
          iter_expression exp1;
          iter_expression exp2
        | Pexp_array list ->
          List.iter iter_expression list
        | Pexp_ifthenelse (exp1, exp2, expo) ->
          iter_expression exp1;
          iter_expression exp2;
          may_iter iter_expression expo
        | Pexp_sequence (exp1, exp2) ->
          iter_expression exp1;
          iter_expression exp2
        | Pexp_while (exp1, exp2) ->
          iter_expression exp1;
          iter_expression exp2
        | Pexp_for (_, exp1, exp2, _dir, exp3) ->
          iter_expression exp1;
          iter_expression exp2;
          iter_expression exp3
        | Pexp_send (exp, _meth) ->
          iter_expression exp;
        | Pexp_new _path -> ()
        | Pexp_setinstvar (_, exp) ->
          iter_expression exp
        | Pexp_override list ->
          List.iter (fun (_path, exp) ->
            iter_expression exp
          ) list
        | Pexp_assert exp -> iter_expression exp
        | Pexp_lazy exp -> iter_expression exp
        | Pexp_object cl ->
          iter_class_structure cl
      end;
      Iter.leave_expression exp;

    and iter_package_type pack =
      Iter.enter_package_type pack;
      List.iter (fun (_s, ct) -> iter_core_type ct) (snd pack);
      Iter.leave_package_type pack;

    and iter_signature sg =
      Iter.enter_signature sg;
      List.iter iter_signature_item sg;
      Iter.leave_signature sg;

    and iter_signature_item item =
      Iter.enter_signature_item item;
      begin
        match item.psig_desc with
#if OCAML_VERSION < "4.02"
        | Psig_value (_, v) -> iter_value_description v
        | Psig_exception (_, decl) -> iter_exception_declaration decl
        | Psig_module (_, mtype) -> iter_module_type mtype
        | Psig_modtype (_, mdecl) -> iter_modtype_declaration mdecl
        | Psig_type list ->
          List.iter (fun (_, decl) ->
            iter_type_declaration decl
          ) list
        | Psig_recmodule list ->
          List.iter (fun (_, mtype) -> iter_module_type mtype) list
        | Psig_include mty -> iter_module_type mty
#else
        | Psig_value v -> iter_value_description v
        | Psig_typext ext -> iter_type_extension ext
        | Psig_exception exn -> iter_extension_constructor exn
        | Psig_module md -> iter_module_type md.pmd_type
        | Psig_recmodule list ->
          List.iter (fun md -> iter_module_type md.pmd_type) list
        | Psig_modtype mtd -> iter_module_type_declaration mtd
        | Psig_extension (ext, _attr) -> iter_extension ext
        | Psig_include incl -> iter_module_type incl.pincl_mod
        | Psig_attribute _attr -> ()
#if OCAML_VERSION < "4.03"
        | Psig_type decl -> List.iter iter_type_declaration decl
#else
        | Psig_type (recflag, list) ->
          Iter.enter_types_declaration recflag;
          List.iter iter_type_declaration list;
          Iter.leave_types_declaration recflag
#endif
#endif
        | Psig_open _ -> ()
        | Psig_class list ->
          List.iter iter_class_description list
        | Psig_class_type list ->
          List.iter iter_class_type_declaration list
      end;
      Iter.leave_signature_item item;


    and iter_class_description cd =
      Iter.enter_class_description cd;
      iter_class_type cd.pci_expr;
      Iter.leave_class_description cd;

    and iter_class_type_declaration cd =
      Iter.enter_class_type_declaration cd;
      iter_class_type cd.pci_expr;
      Iter.leave_class_type_declaration cd;

    and iter_module_type mty =
      Iter.enter_module_type mty;
      begin
        match mty.pmty_desc with
          Pmty_ident _lid -> ()
        | Pmty_signature sg -> iter_signature sg
#if OCAML_VERSION < "4.02"
        | Pmty_functor (_, mtype1, mtype2) ->
          iter_module_type mtype1; iter_module_type mtype2
        | Pmty_with (mtype, list) ->
          iter_module_type mtype;
          List.iter (fun (path, withc) ->
            iter_with_constraint withc
          ) list
        | Pmty_typeof mexpr ->
          iter_module_expr mexpr
#else
        | Pmty_functor (_loc, mtypeo, mtype) ->
          may_iter iter_module_type mtypeo;
          iter_module_type mtype
        | Pmty_with (mtype, withc) ->
          iter_module_type mtype;
          List.iter iter_with_constraint withc
        | Pmty_typeof mexpr -> iter_module_expr mexpr
        | Pmty_extension ext -> iter_extension ext
        | Pmty_alias _loc -> ()
#endif
      end;
      Iter.leave_module_type mty;

    and iter_with_constraint cstr =
      Iter.enter_with_constraint cstr;
      begin
        match cstr with
#if OCAML_VERSION < "4.02"
        | Pwith_type decl -> iter_type_declaration decl
#else
        | Pwith_type (_, decl) -> iter_type_declaration decl
#endif
        | Pwith_module _ -> ()
#if OCAML_VERSION < "4.06"
        | Pwith_typesubst decl -> iter_type_declaration decl
#else
        | Pwith_typesubst (loc,decl) -> iter_type_declaration decl
#endif
        | Pwith_modsubst _ -> ()
      end;
      Iter.leave_with_constraint cstr;

    and iter_module_expr mexpr =
      Iter.enter_module_expr mexpr;
      begin
        match mexpr.pmod_desc with
          Pmod_ident _p -> ()
        | Pmod_structure st -> iter_structure st
#if OCAML_VERSION < "4.02"
        | Pmod_functor (_, mtype, mexpr) ->
          iter_module_type mtype;
          iter_module_expr mexpr
#else
        | Pmod_functor (_loc, mtypeo, mexpr) ->
          may_iter iter_module_type mtypeo;
          iter_module_expr mexpr
        | Pmod_extension ext -> iter_extension ext
#endif
        | Pmod_apply (mexp1, mexp2) ->
          iter_module_expr mexp1;
          iter_module_expr mexp2
        | Pmod_constraint (mexpr, mod_type ) ->
          iter_module_expr mexpr;
          iter_module_type mod_type
        | Pmod_unpack exp ->
          iter_expression exp
      (*          iter_module_type mty *)
      end;
      Iter.leave_module_expr mexpr;

    and iter_class_expr cexpr =
      Iter.enter_class_expr cexpr;
      begin
        match cexpr.pcl_desc with
#if OCAML_VERSION < "4.02"
#else
        | Pcl_extension ext -> iter_extension ext
#endif
        | Pcl_constraint (cl, ct) ->
          iter_class_expr cl;
          iter_class_type ct
        | Pcl_structure clstr -> iter_class_structure clstr
        | Pcl_fun (_label, expo, pat, cl) ->
          may_iter iter_expression expo;
          iter_pattern pat;
          iter_class_expr cl

        | Pcl_apply (cl, args) ->
          iter_class_expr cl;
          List.iter (fun (_label, exp) ->
            iter_expression exp
          ) args

        | Pcl_let (rec_flat, bindings, cl) ->
          iter_bindings false rec_flat bindings;
          iter_class_expr cl

        | Pcl_constr (_, tyl) ->
          List.iter iter_core_type tyl
#if OCAML_VERSION >= "4.06"
      | Pcl_open (ovflag, lid, ct) -> iter_class_expr ct
#endif
      end;
      Iter.leave_class_expr cexpr;

    and iter_class_type ct =
      Iter.enter_class_type ct;
      begin
        match ct.pcty_desc with
          Pcty_signature csg -> iter_class_signature csg
        | Pcty_constr (_path, list) ->
          List.iter iter_core_type list
#if OCAML_VERSION < "4.02"
        | Pcty_fun (_label, ct, cl) ->
          iter_core_type ct;
          iter_class_type cl
#else
        | Pcty_arrow (_label, ct, cl) ->
          iter_core_type ct;
          iter_class_type cl
        | Pcty_extension ext -> iter_extension ext
#endif
#if OCAML_VERSION >= "4.06"
      | Pcty_open (ovflag, lid, ct) -> iter_class_type ct
#endif
      end;
      Iter.leave_class_type ct;

    and iter_class_signature cs =
      Iter.enter_class_signature cs;
      iter_core_type cs.pcsig_self;
      List.iter iter_class_type_field cs.pcsig_fields;
      Iter.leave_class_signature cs


    and iter_class_type_field ctf =
      Iter.enter_class_type_field ctf;
      begin
        match ctf.pctf_desc with
#if OCAML_VERSION < "4.02"
        | Pctf_inher ct -> iter_class_type ct
        | Pctf_virt  (s, priv, ct) -> iter_core_type ct
        | Pctf_meth  (s, priv, ct) -> iter_core_type ct
        | Pctf_cstr  (ct1, ct2) ->
          iter_core_type ct1;
          iter_core_type ct2
#else
        | Pctf_inherit cl -> iter_class_type cl
        | Pctf_method (_s, _priv, _virt, ct) -> iter_core_type ct
        | Pctf_constraint (ct1, ct2) ->
          iter_core_type ct1;
          iter_core_type ct2
        | Pctf_extension ext -> iter_extension ext
        | Pctf_attribute _attr -> ()
#endif
        | Pctf_val (_s, _mut, _virt, ct) ->
          iter_core_type ct
      end;
      Iter.leave_class_type_field ctf

    and iter_core_type ct =
      Iter.enter_core_type ct;
      begin
        match ct.ptyp_desc with
          Ptyp_any -> ()
#if OCAML_VERSION < "4.02"
        | Ptyp_object list ->
          List.iter iter_core_field_type list
        | Ptyp_class (path, list, _labels) ->
          List.iter iter_core_type list
#else
#if OCAML_VERSION < "4.06"
        | Ptyp_object (list, _close_flag) ->
          List.iter (fun (_s, _attr, ct) -> iter_core_type ct) list
#else
        | Ptyp_object (list, _close_flag) ->
           List.iter (function
                      | Otag (_s, _attr, ct) -> iter_core_type ct
                      | Oinherit ct -> iter_core_type ct
                     ) list
#endif


        | Ptyp_class (_loc, list) -> List.iter iter_core_type list
        | Ptyp_extension ext -> iter_extension ext
#endif
        | Ptyp_var _s -> ()
        | Ptyp_arrow (_label, ct1, ct2) ->
          iter_core_type ct1;
          iter_core_type ct2
        | Ptyp_tuple list -> List.iter iter_core_type list
        | Ptyp_constr (_path, list) ->
          List.iter iter_core_type list
        | Ptyp_alias (ct, _s) ->
          iter_core_type ct
        | Ptyp_variant (list, _bool, _labels) ->
          List.iter iter_row_field list
        | Ptyp_poly (_list, ct) -> iter_core_type ct
        | Ptyp_package pack -> iter_package_type pack
      end;
      Iter.leave_core_type ct;

    and iter_class_structure cs =
      Iter.enter_class_structure cs;
#if OCAML_VERSION < "4.02"
      iter_pattern cs.pcstr_pat;
#else
      iter_pattern cs.pcstr_self;
#endif
      List.iter iter_class_field cs.pcstr_fields;
      Iter.leave_class_structure cs;


    and iter_row_field rf =
      match rf with
#if OCAML_VERSION < "4.02"
      | Rtag (_label, bool, list) ->
          List.iter iter_core_type list
#else
      | Rtag (_label, _attr, _flag, list) -> List.iter iter_core_type list
#endif
      | Rinherit ct -> iter_core_type ct

    and iter_class_field cf =
      Iter.enter_class_field cf;
      begin
        match cf.pcf_desc with
#if OCAML_VERSION < "4.02"
        | Pcf_inher (ovf, cl, super) -> iter_class_expr cl
        | Pcf_constr (cty, cty') ->
          iter_core_type cty;
          iter_core_type cty'
        | Pcf_valvirt (name, mut, cty) -> iter_core_type cty
        | Pcf_val (name, mut, override, exp) -> iter_expression exp
        | Pcf_virt (name, mut, cty) -> iter_core_type cty
        | Pcf_meth (name, priv, override, exp) -> iter_expression exp
        | Pcf_init exp -> iter_expression exp
#elif OCAML_VERSION >= "4.02"
        | Pcf_inherit (_ovf, cl, _super) -> iter_class_expr cl
        | Pcf_val (_loc, _mflag, kind) -> iter_class_field_kind kind
        | Pcf_method (_loc, _priv, kind) -> iter_class_field_kind kind
        | Pcf_constraint (ct1, ct2) ->
          iter_core_type ct1;
          iter_core_type ct2
        | Pcf_initializer exp -> iter_expression exp
        | Pcf_extension ext -> iter_extension ext
        | Pcf_attribute _attr -> ()
#endif
      end;
      Iter.leave_class_field cf;

#if OCAML_VERSION < "4.02"
    and iter_binding toplevel (pat, exp) =
      Iter.enter_binding toplevel pat exp;
      iter_pattern pat;
      iter_expression exp;
      Iter.leave_binding toplevel pat exp
#endif

    and iter_bindings toplevel rec_flag list =
      Iter.enter_bindings rec_flag;
#if OCAML_VERSION < "4.02"
      List.iter (iter_binding toplevel) list;
#else
      List.iter (iter_value_binding toplevel) list;
#endif
      Iter.leave_bindings rec_flag

#if OCAML_VERSION < "4.02"
    and iter_exception_declaration decl =
      Iter.enter_exception_declaration decl;
      List.iter iter_core_type decl;
      Iter.leave_exception_declaration decl;

    and iter_modtype_declaration mdecl =
      Iter.enter_modtype_declaration mdecl;
      begin
        match mdecl with
          Pmodtype_abstract -> ()
        | Pmodtype_manifest mtype -> iter_module_type mtype
      end;
      Iter.leave_modtype_declaration mdecl;

    and iter_core_field_type cft =
      Iter.enter_core_field_type cft;
      begin match cft.pfield_desc with
        Pfield_var -> ()
      | Pfield (s, ct) -> iter_core_type ct
      end;
      Iter.leave_core_field_type cft;
#else

    and iter_value_binding toplevel v =
      Iter.enter_binding toplevel v.pvb_pat v.pvb_expr;
      iter_pattern v.pvb_pat;
      iter_expression v.pvb_expr;
      Iter.leave_binding toplevel v.pvb_pat v.pvb_expr;

#endif

  end

module DefaultIteratorArgument = struct

#if OCAML_VERSION < "4.02"
      let enter_exception_declaration _ = ()
      let leave_exception_declaration _ = ()
      let enter_modtype_declaration _ = ()
      let leave_modtype_declaration _ = ()
      let enter_core_field_type _ = ()
      let leave_core_field_type _ = ()
#else
      let enter_type_extension _ = ()
      let leave_type_extension _ = ()
      let enter_extension_constructor _ = ()
      let leave_extension_constructor _ = ()
      let enter_module_type_declaration _ = ()
      let leave_module_type_declaration _ = ()
#endif

#if OCAML_VERSION >= "4.03"
     let enter_types_declaration _ = ()
     let leave_types_declaration _ = ()
#endif

      let enter_structure _ = ()
      let enter_value_description _ = ()
      let enter_type_declaration _ = ()
      let enter_pattern _ = ()
      let enter_expression _ = ()
      let enter_package_type _ = ()
      let enter_signature _ = ()
      let enter_signature_item _ = ()
      let enter_module_type _ = ()
      let enter_module_expr _ = ()
      let enter_with_constraint _ = ()
      let enter_class_expr _ = ()
      let enter_class_signature _ = ()
      let enter_class_declaration _ = ()
      let enter_class_description _ = ()
      let enter_class_type_declaration _ = ()
      let enter_class_type _ = ()
      let enter_class_type_field _ = ()
      let enter_core_type _ = ()
      let enter_class_structure _ = ()
    let enter_class_field _ = ()
    let enter_structure_item _ = ()



      let leave_structure _ = ()
      let leave_value_description _ = ()
      let leave_type_declaration _ = ()
      let leave_pattern _ = ()
      let leave_expression _ = ()
      let leave_package_type _ = ()
      let leave_signature _ = ()
      let leave_signature_item _ = ()
      let leave_module_type _ = ()
      let leave_module_expr _ = ()
      let leave_with_constraint _ = ()
      let leave_class_expr _ = ()
      let leave_class_signature _ = ()
      let leave_class_declaration _ = ()
      let leave_class_description _ = ()
      let leave_class_type_declaration _ = ()
      let leave_class_type _ = ()
      let leave_class_type_field _ = ()
      let leave_core_type _ = ()
      let leave_class_structure _ = ()
    let leave_class_field _ = ()
    let leave_structure_item _ = ()

    let enter_binding _ _ _ = ()
    let leave_binding _ _ _ = ()

    let enter_bindings _ = ()
    let leave_bindings _ = ()

  end

let iter_structure iterator structure =
  let module IA = (val iterator : IteratorArgument) in
  let module I = (MakeIterator(IA)) in
  I.iter_structure structure

let iter_signature iterator signature =
  let module IA = (val iterator : IteratorArgument) in
  let module I = (MakeIterator(IA)) in
  I.iter_signature signature
