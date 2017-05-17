let forbidden_modules = [
  Path.Pident (Ident.create "Obj");
  Path.Pident (Ident.create "Marshal");
  Path.Pident (Ident.create "Printexc");
]

let details =
  "Check if some forbidden functions are used. The functions that can not be used are the next : " ^  List.fold_left (fun x y -> x ^ "\n\t- " ^ Path.name y) "" forbidden_modules
  
module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Check Use Of Forbidden Modules"
    let version = "1"
    let short_name = "check_forbidden_modules"
    let details = details
    let enable = true
  end)

type warning =
  | FunctionInForbiddenModule of Path.t
  | OpenForbiddenModule of Path.t
				   
let w_function_in_forbidden_mod = Linter.new_warning
    ~id:1
    ~short_name:"function_in_forbidden_module"
    ~msg:"$fun is in a forbidden module."
    ~severity:1

let w_open_forbidden_mod = Linter.new_warning
    ~id:1
    ~short_name:"open_forbidden_module"
    ~msg:"$mod is a forbidden module."
    ~severity:1
    
module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | FunctionInForbiddenModule p ->
	 w_function_in_forbidden_mod, [("fun",Path.name p)]
      | OpenForbiddenModule p ->
	 w_open_forbidden_mod, [("mod",Path.name p)]
  end)

let iter =
  let module IterArg = struct
    include Typedtree_iter.DefaultIteratorArgument

    open Typedtree
    open Asttypes 
    open Path

    let rec same_path p1 p2 =
      match (p1, p2) with
      |	(Pident id1, Pident id2) -> Ident.equal id1 id2
      | (Pdot(p1, s1, _pos1), Pdot(p2, s2, _pos2)) -> s1 = s2 && same_path p1 p2
      | (Papply(fun1, arg1), Papply(fun2, arg2)) ->
	 same_path fun1 fun2 && same_path arg1 arg2
      | (_, _) -> false

    let is_forbidden m =
      List.exists (same_path m) forbidden_modules 	  
	       
    let enter_expression expr =
      let use_forbidden_module fpath =
	let rec aux =
	  function
	  | Pdot (Pident id as p',_,_) -> p'
	  | Pdot (Pdot (_,s,n) as p',_,_) -> Pdot (aux p',s,n)
	  | _ -> failwith "err"
	in
	match fpath with
	| Pdot (Pdot _,_,_) | Pdot (Pident _,_,_) -> is_forbidden (aux fpath)
	| _ -> false
      in
      begin match expr.exp_desc with
	    | Texp_apply (f,_) ->
	       begin match f.exp_desc with
		     | Texp_ident (path,_,_) when use_forbidden_module path ->
			Warnings.report f.exp_loc (FunctionInForbiddenModule path)
		     | _ -> ()
	       end
	    | _ -> ()
      end ;
      List.iter
	begin function extra,loc,_ ->
	   match extra with
	   | Texp_open (_,p,_,_) when is_forbidden p -> Warnings.report loc (OpenForbiddenModule p)
	   | _ -> ()
	end expr.exp_extra

    let enter_pattern pat =
      List.iter
	begin function extra,loc,_ ->
	   match extra with
	   | Tpat_open (p,_,_) when is_forbidden p -> Warnings.report loc (OpenForbiddenModule p)
	   | _ -> ()
	end pat.pat_extra

    let enter_structure_item str =
      match str.str_desc with
      | Tstr_open od when is_forbidden od.open_path ->
	 Warnings.report od.open_loc (OpenForbiddenModule od.open_path)
      | _ -> ()

    let enter_signature_item si =
      match si.sig_desc with
      | Tsig_open od when is_forbidden od.open_path ->
	 Warnings.report od.open_loc (OpenForbiddenModule od.open_path)
      | _ -> ()
	
  end in
  (module IterArg : Typedtree_iter.IteratorArgument)
    
module MainML = Linter.MakeInputCMT(struct
    let main cmt = Typedtree_iter.iter_structure iter cmt
  end)
