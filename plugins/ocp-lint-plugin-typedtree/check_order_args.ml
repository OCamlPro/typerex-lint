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
    ~msg:"'$func' have arguments with same types, ($good) -> $err_msg"
    ~severity:9

let err_file ?file:(file = "err.txt") str = 
	let oc = open_out file in Printf.fprintf oc "(%s) \n" str;
	close_out oc;;

let rec concat_el l acc = match l with
  | [] -> acc
  | head::tail -> concat_el tail (acc^" "^head)


let rec key_exist l key = match l with
  [] -> false
  | (typ,_,_)::tl -> if key = typ then true else key_exist tl key

let rec create_keys l acc = match l with
  [] -> acc
  | (label,typ) :: tl -> if(key_exist acc typ) then create_keys tl acc else create_keys tl ([(typ,0,0)] @ acc)


let rec cumulative l map = match l with
  |[] -> map
  |(label,typ) :: tl -> cumulative tl (List.map (function (t,occur,occur_label) -> if typ = t then 
                                                                                    if label = "labelled" then 
                                                                                      (t,occur+1,occur_label+1) 
                                                                                    else
                                                                                      (t,occur+1,occur_label)
                                                                                   else
                                                                                     (t,occur,occur_label)) map)


module Warnings = Linter.MakeWarnings(struct
    type t = warning
    let to_warning = function
      | BadUseArg (good_order,func,types) -> w_badusearg, [("good",good_order );("func",func);("err_msg",types)]
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


(*
   t -> le type_expr desc
   types -> la liste de couple (type,label) avec label qui peut prendre 3 valeur : nolabel, labelled, optional
   label -> la variable temporaire qui sert a stocker le label courant (vu que la fonction est recursive j'ai besoin de garder cette valeur la jusqu'au cas terminaux ou je l'ajoute a la liste (type,label)

*)

let rec matching_type t map_types label = let open Typedtree in let open Types in match t.desc  with
  | Tvar (data) ->  map_types @ [(label,get_string data)]
  | Tarrow (arg_label,arg_type,result_type,_) ->   let arg_type = matching_type arg_type [] (get_arg_label arg_label) in
                                                   let map_types = map_types @ arg_type in matching_type result_type map_types ""

  | Ttuple (t_list) ->  get_type_args t_list map_types label
  | Tconstr (path,t_list,_) -> map_types @ [(label,check_path path "")]
  | Tobject (_,_) -> [("","tobject")]
  | Tfield (_,_,_,_) -> [("","tfield")]
  | Tnil  -> [("","tnil")]
  | Tlink (link) ->  matching_type link map_types label
  | Tsubst (_) -> [("","tsubst")]
  | Tvariant (_) -> [("","tvariant")]
  | Tunivar (_) -> [("","tunivar")]
  | Tpoly (_,_) -> [("","tpoly")]
  | Tpackage (_,_,_) -> [("","tpackage")]

(* 
   Fonction mutuellement recursive avec matching type, car dans le cas de Tconstr on peut avoir a parser une liste de type_expr donc je l'envoie ici.
*)

and get_type_args t acc label = let open Typedtree in let open Types in match t with
  | [] ->  acc
  | hd::tl ->  get_type_args tl (matching_type hd acc label) label


let get_type_func val_bind = let open Typedtree in match val_bind.vb_expr.exp_type.desc with
  | Tvar (data) -> [("","tvar")]
  | Tarrow (arg_label,arg_type,result_type,commut) -> let arg_type = matching_type arg_type [] (get_arg_label arg_label) in let map_types = arg_type @ [] in matching_type result_type map_types ""
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

(*
   cette fonction sert a me couper la liste (type,label) pour ne garder que les type des arguments et non le type de sortie, car quand je parse le type de la fonction je récupère tout (argument,sortie).
   n elements de la liste l sont transféré dans le tableau acc.
*)

let rec sub_list l n acc = match n with
    | 0 -> acc
    | _ -> sub_list (List.tl l) (n-1) (acc @ [List.hd l]) 

let  get_args_name val_bind = let open Typedtree in match val_bind.vb_expr.exp_desc with
  | Texp_function (record) -> iter_cases (List.hd record.cases) []
  | _ -> []

let get_args_type val_bind = let open Typedtree in match val_bind.vb_expr.exp_desc with
  | Texp_function (record) ->  sub_list (get_type_func val_bind) (List.length (iter_cases(List.hd record.cases) [])) []
  | _ -> []

let rec get_err_msg assoc acc = match assoc with
  [] -> acc
  | (typ,occur_t,occur_label)::tl -> if occur_t > occur_label then get_err_msg tl (acc ^""^string_of_int(occur_t)^" "^typ^" found but "^string_of_int(occur_label)^" labelled. ") else get_err_msg tl acc

let iter =
  let module IterExp = struct
    include Typedtree_iter.DefaultIteratorArgument

    let enter_structure_item strct =
      let open Typedtree in 
      let open Asttypes in 


      begin match strct.str_desc with
        | Tstr_value (rec_flag, binding) -> List.iter (function binding -> let args_name = get_args_name binding in let args_type = get_args_type binding in 
          let (txt,loc) = get_func_name binding in 
          let assoc_t  = cumulative args_type (create_keys args_type []) in
          if((get_err_msg assoc_t "") <> "None")
            then
              Warnings.report loc (BadUseArg ((concat_el args_name ""),txt,(get_err_msg assoc_t "")))) binding
        | _ -> ()
      end
  end in
  (module IterExp : Typedtree_iter.IteratorArgument)

module Main = Linter.MakeInputCMT(struct 
    let main cmt = Typedtree_iter.iter_structure iter cmt
end)
