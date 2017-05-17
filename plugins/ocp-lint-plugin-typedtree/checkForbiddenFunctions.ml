let forbidden_functions = [
  Path.Pdot (Path.Pdot (Path.Pident (Ident.create "Bigarray"),"Array1",1), "unsafe_get", 2);
  Path.Pdot (Path.Pdot (Path.Pident (Ident.create "Bigarray"),"Array1",1), "unsafe_set", 2);
  Path.Pdot (Path.Pdot (Path.Pident (Ident.create "Bigarray"),"Array2",1), "unsafe_get", 2);
  Path.Pdot (Path.Pdot (Path.Pident (Ident.create "Bigarray"),"Array2",1), "unsafe_set", 2);
  Path.Pdot (Path.Pdot (Path.Pident (Ident.create "Bigarray"),"Array3",1), "unsafe_get", 2);
  Path.Pdot (Path.Pdot (Path.Pident (Ident.create "Bigarray"),"Array3",1), "unsafe_set", 2);
  Path.Pdot (Path.Pident (Ident.create "Bytes"),"unsafe_to_string",1);
  Path.Pdot (Path.Pident (Ident.create "Clflags"),"create",1);
  Path.Pdot (Path.Pident (Ident.create "Dynlink"),"reset",1);
  Path.Pdot (Path.Pident (Ident.create "Dynlink"),"add_interfaces",1);
  Path.Pdot (Path.Pident (Ident.create "Dynlink"),"add_variables_units",1);
  Path.Pdot (Path.Pident (Ident.create "Dynlink"),"loadfile",1);
  Path.Pdot (Path.Pident (Ident.create "Dynlink"),"loadfile_private",1);
  Path.Pdot (Path.Pdot (Path.Pident (Ident.create "Misc"),"LongString",1), "unsafe_blit_to_byte", 2);
]

let deprecated_functions = [
  (Path.Pdot (Path.Pident (Ident.create "String"),"create",1)),(Path.Pdot (Path.Pident (Ident.create "String"),"make",1));
]
			    
let details =
  let strff = List.map Path.name forbidden_functions
  and strdf = List.map (fun (x,_) -> Path.name x) deprecated_functions in
  let str = List.fold_left (fun x y -> x ^ "\n\t- " ^ y) "" (strff @ strdf) in
  "Check if some forbidden functions are used. The functions that can not be used are the next : " ^ str
			    
module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Check Use Of Forbidden Functions"
    let version = "1"
    let short_name = "check_forbidden_functions"
    let details = details 
    let enable = true
  end)

type warning =
  | UseForbiddenFunction of Path.t
  | UseDeprecatedFunction of Path.t * Path.t

let w_forbidden_fun = Linter.new_warning
    ~id:1
    ~short_name:"forbidden_function"
    ~msg:"$fun is a forbidden function."
    ~severity:1

let w_deprecated_fun = Linter.new_warning
    ~id:2
    ~short_name:"deprecated_function"
    ~msg:"$old is a deprecated function, use $new."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | UseForbiddenFunction p ->
	 w_forbidden_fun, [("fun",Path.name p)]
      | UseDeprecatedFunction (p,p') ->			    
	 w_deprecated_fun, [("old",Path.name p);("new", Path.name p')]
			    
end)

let iter =
  let module IterArg = struct
    include Typedtree_iter.DefaultIteratorArgument

    let enter_expression expr =
      let open Typedtree in
      let open Asttypes in 
      let open Path in
      let rec same_path p1 p2 =
	match (p1, p2) with
	  (Pident id1, Pident id2) -> Ident.equal id1 id2
	| (Pdot(p1, s1, _pos1), Pdot(p2, s2, _pos2)) -> s1 = s2 && same_path p1 p2
	| (Papply(fun1, arg1), Papply(fun2, arg2)) ->
	   same_path fun1 fun2 && same_path arg1 arg2
	| (_, _) -> false
      in
      let is_forbidden fpath =
	List.exists (same_path fpath) forbidden_functions
      in
      let newer_function fpath =
        let rec aux = function
	  | [] -> None
	  | (df,nf) :: _ when same_path fpath df -> Some nf
	  | _ :: tl -> aux tl
	in aux deprecated_functions
      in
      begin match expr.exp_desc with
	    | Texp_apply (f,_) ->
	       begin match f.exp_desc with
		     | Texp_ident (path,_,_) when is_forbidden path ->
			Warnings.report f.exp_loc (UseForbiddenFunction path);
		     | Texp_ident (path,_,_) ->
			begin match newer_function path with
			      | Some nf ->  Warnings.report f.exp_loc (UseDeprecatedFunction (path,nf));
			      | None -> ()
			end
		     | _ -> ()
	       end
	    | _ -> ()
      end
  end in
  (module IterArg : Typedtree_iter.IteratorArgument)
    
module MainML = Linter.MakeInputCMT(struct
    let main cmt = Typedtree_iter.iter_structure iter cmt
  end)
