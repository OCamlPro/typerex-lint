module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Detect bad use of arguments when functions are called"
    let version = "1"
    let short_name = "arg_func_order"
    let details = "check if function called, are called with their arguments in good order"
    let enabled = true;
  end)

type warning = BadUseArg of (string * string * string)



let w_badusearg = Linter.new_warning
    ~id:1
    ~short_name:"arg_func_order"
    ~msg:"the function '$func' with parameters ($good) and their types ($types)"
    ~severity:9

let err_file ?file:(file = "err.txt") str = 
	let oc = open_out file in Printf.fprintf oc "(%s) \n" str;
	close_out oc;;



let rec concat_el l acc = match l with
  | [] -> acc
  | head::tail -> concat_el tail (acc^" "^head)


let rec count_same_type l acc = match l with
  | [] -> acc
  | hd :: tl -> count_same_type tl (acc + compare_type tl hd 0)

and compare_type l el acc = match l with
  | [] -> acc
  | hd::tl -> if(hd = el) then compare_type tl el (acc+1) else compare_type tl el acc

let rec count_labeled_args l acc = match l with
  | [] -> acc
  | hd::tl -> if(hd = "nolabel") then count_labeled_args tl (acc+1) else count_labeled_args tl (acc)

let rec if_need_label l acc_type acc_label = match l with
  |[] -> (acc_type,acc_label)
  |(label,typ) :: tl -> if_need_label tl (acc_type @ [typ]) (acc_label @ [label])



let printargs l = match l with
  | hd::tl -> Printf.printf "%s \n" hd
  | _ -> Printf.printf "%s \n" ""

module Warnings = Linter.MakeWarnings(struct
    type t = warning
    let to_warning = function
      | BadUseArg (good_order,func,types) -> w_badusearg, [("good",good_order );("func",func);("types",types)]
  end)

let get_string data = match data with
  | None -> "none"
  | Some str -> str

let rec check_path p acc = let open Path in match p with
  | Pident (ident) -> acc ^ ident.name
  | Pdot (t,str,ent) -> check_path t acc^str
  | Papply (t_,t__) -> "Papply"


let get_arg_label arg = let open Asttypes in match arg with
  | Nolabel -> "nolabel"
  | Labelled (str) -> "labelled"
  | Optional (str) -> "optional"


let rec matching_type t types label = let open Typedtree in let open Types in match t.desc  with
  | Tvar (data) -> Printf.printf "-%s %! \n" label; types @ [(label,get_string data)]
  | Tarrow (arg_label,t_exp,t__exp,_) -> Printf.printf "-----%s %! \n" label; matching_type t__exp (matching_type t_exp types (get_arg_label arg_label)) (get_arg_label arg_label)
  | Ttuple (t_list) -> Printf.printf "--%s %! \n" label; get_type_args t_list types label
  | Tconstr (path,t_list,_) -> Printf.printf "---%s %s %! \n" label (check_path path ""); types @ [(label,check_path path "")]
  | Tobject (_,_) -> [("","tobject")]
  | Tfield (_,_,_,_) -> [("","tfield")]
  | Tnil  -> [("","tnil")]
  | Tlink (link) -> Printf.printf "----%s %! \n" label; matching_type link types label
  | Tsubst (_) -> [("","tsubst")]
  | Tvariant (_) -> [("","tvariant")]
  | Tunivar (_) -> [("","tunivar")]
  | Tpoly (_,_) -> [("","tpoly")]
  | Tpackage (_,_,_) -> [("","tpackage")]

and get_type_args t acc label = let open Typedtree in let open Types in match t with
  | [] -> acc
  | hd::tl -> get_type_args tl (matching_type hd acc label) label

let get_type_func val_bind = let open Typedtree in match val_bind.vb_expr.exp_type.desc with
  | Tvar (data) -> [("","tvar")]
  | Tarrow (arg_label,t_exp,t__exp,commut) -> matching_type t_exp (matching_type t__exp [] "") ""
  | Ttuple (t_list) ->  [("","ttuple")]
  | Tconstr (path,t_list,_) -> [("","tconstr")]
  | Tobject (_,_) -> [("","tobject")]
  | Tfield (_,_,_,_) -> [("","tfield")]
  | Tnil  -> [("","tnil")]
  | Tlink (link) -> matching_type link [] ""
  | Tsubst (_) -> [("","tsubst")]
  | Tvariant (_) -> [("","tvariant")]
  | Tunivar (_) -> [("","tunivar")]
  | Tpoly (_,_) -> [("","tpoly")]
  | Tpackage (_,_,_) -> [("","tpackage")]

let get_func_name pat = let open Typedtree in let open Asttypes in match pat.vb_pat.pat_desc with
  | Tpat_var (ident,name_loc) ->  (name_loc.txt,name_loc.loc)
  | _ -> ("",pat.vb_loc)

let next_case case = let open Typedtree in match case.c_rhs.exp_desc with
  | Texp_function (record) -> List.hd record.cases
  | _ -> case

let rec iter_cases case acc = let open Typedtree in match case.c_lhs.pat_desc with
  | Tpat_var (ident,loc) -> if(next_case(case) = case) then acc @ [ident.name] else iter_cases (next_case(case)) (acc @ [ident.name])
  | _ -> acc


let rec sub_list l n acc = match n with
    | 0 -> acc
    | _ -> sub_list (List.tl l) (n-1) (acc @ [List.hd l])

let  get_args_name val_bind = let open Typedtree in match val_bind.vb_expr.exp_desc with
  | Texp_function (record) -> iter_cases (List.hd record.cases) []
  | _ -> []

let get_args_type val_bind = let open Typedtree in match val_bind.vb_expr.exp_desc with
  | Texp_function (record) -> sub_list (get_type_func val_bind) (List.length (iter_cases(List.hd record.cases) [])) []
  | _ -> []


let iter =
  let module IterExp = struct
    include Typedtree_iter.DefaultIteratorArgument

    let enter_structure_item strct =
      let open Typedtree in 
      let open Asttypes in 
      begin match strct.str_desc with
        | Tstr_value (rec_flag, binding) -> let args_name = get_args_name (List.hd binding) in let args_type = get_args_type (List.hd binding) in 
          let (txt,loc) = get_func_name (List.hd binding) in let (types,label)  = if_need_label args_type [] [] in let same_type = (count_same_type types 0) in let labeled = (count_labeled_args label 0) in Warnings.report loc (BadUseArg ((concat_el args_name ""),txt, concat_el label ""))
        | _ -> ()
      end
  end in
  (module IterExp : Typedtree_iter.IteratorArgument)

module Main = Linter.MakeInputCMT(struct 
    let main cmt = Typedtree_iter.iter_structure iter cmt
end)
