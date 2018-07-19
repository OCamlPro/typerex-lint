module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Detects lengthy tuples.Rationale: When a tuple has too many members, it should be refactored into a record"
    let version = "1"
    let short_name = "lengthy_tuples"
    let details = "blabla"
    let enabled = true;
  end)

type warning = TupleTooLong of (string * int)

let limit = 2;;


let w_toolongtuple = Linter.new_warning
    ~id:1
    ~short_name:"tuple too long"
    ~msg:"the tuple '$var' have '$params' elements, the limit is $limit, When a tuple has too many members, it should be refactored into a record"
    ~severity:5


module Warnings = Linter.MakeWarnings(struct
    type t = warning
    let to_warning = function
      | TupleTooLong (var,nb_parms) -> w_toolongtuple, [("var",var);("params",string_of_int nb_parms);("limit",string_of_int limit)]

  end)


let get_pattern pat = 
  let open Typedtree in
  let open Location in 
  begin
    match pat.vb_pat.pat_desc with
    | Tpat_var (ident,var) -> (var.loc,var.txt)
    | _ -> (pat.vb_loc,"")
  end


let iter_exp exp = 
  let open Typedtree in
  begin
    match exp.vb_expr.exp_desc with
    | Texp_tuple l ->  List.length l
    | _ -> 0
  end


let check_length_tuples bind = if ((iter_exp bind) > limit) then let (loc,txt) = get_pattern bind in Warnings.report loc (TupleTooLong(txt,iter_exp bind))


let iter =
  let module IterExp = struct
    open Typedtree
    open Asttypes
    include Typedtree_iter.DefaultIteratorArgument

    let enter_structure_item strct = 
      begin match strct.str_desc with
        | Tstr_value (rec_flag, binding) -> List.iter check_length_tuples binding
        | _ -> ()
      end
  end in 
  (module IterExp : Typedtree_iter.IteratorArgument)

module Main = Linter.MakeInputCMT(struct
    let main cmt = Typedtree_iter.iter_structure iter cmt
end)
