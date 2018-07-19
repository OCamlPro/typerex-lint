module Linter = Plugin_typedtree.Plugin.MakeLint(struct

		let name = "justify use of ref/mutable field"
		let version = "1"
		let short_name = "mut_ref"
		let details = "Justifier l'utilisation de valeurs mutables. Les protéger par un des mécanismes d'encapsulation recommandés"
		let enabled = true;
	end)

type warning = MutableField of string | Ref_use of string

let w_mutable = Linter.new_warning
	~id:1
    ~short_name:"mutable use"
    ~msg:"you use a mutable field '$mut'"
    ~severity:3

let w_reference = Linter.new_warning
    ~id:2
    ~short_name:"ref use"
    ~msg: "you use a ref with $p"
    ~severity:3


module Warnings = Linter.MakeWarnings(struct
	type t = warning 

	let to_warning = function
		| MutableField mut -> w_mutable, ["mut",mut]
        | Ref_use rf -> w_reference, ["p",rf]
end)


let err_file funct = 
	let oc = open_out "err.txt" in Printf.fprintf oc "im in %d " funct;
	close_out oc;;

let if_mutable lab = 
	let open Asttypes in
	begin match lab with
	| Immutable -> false
	| Mutable -> true
	end;;

let parse_list l = 
	let open Typedtree in 
	begin match l.typ_kind with
	| Ttype_record v -> let p = List.hd v in if (if_mutable p.ld_mutable) 
		then Warnings.report p.ld_name.loc (MutableField p.ld_name.txt) else ()
	| _ -> ()
	end

let longident_ li = 
     let open Longident in 
     begin match li with
       | Lident v -> v
       | _ -> "pattern"
     end

let parse_exp_ident exp = 
     let open Typedtree in
     begin match exp.exp_desc with
     | Texp_ident (path_t,longident,val_desc) -> let name = longident_ (longident.txt)  in let loc = longident.loc in Warnings.report loc ( Ref_use name)
     | _ -> ()
     end


let parse_val_binding vb = 
	let open Typedtree in 
	begin match vb.vb_expr.exp_desc with
	| Texp_apply (exp,l) -> parse_exp_ident exp (*Texp_apply of exp * (arg_label * exp_opt) list*)
	| _ -> ()
        end


let iter =
	let module IterExp = struct
	  open Typedtree
	  open Asttypes
	  include Typedtree_iter.DefaultIteratorArgument

	  let enter_structure_item stru = match stru.str_desc with
	  	| Tstr_type (rec_flag,type_decl) -> List.iter parse_list type_decl
	  	| Tstr_value (rec_flag,val_binding) ->  List.iter parse_val_binding val_binding
	  	| _ -> ()

	 end in 
	 (module IterExp : Typedtree_iter.IteratorArgument)

module Main = Linter.MakeInputCMT(struct
    let main cmt = Typedtree_iter.iter_structure iter cmt
  end)
