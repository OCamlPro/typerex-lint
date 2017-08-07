let unrecommanded_modules = [
  Path.Pident (
      Ident.create "Obj"
    );
  Path.Pident (
      Ident.create "Marshal"
    );
  Path.Pident (
      Ident.create "Printexc"
    );
]

let environment_modification_modules = [
  Path.Pident (
      Ident.create "Parsing"
    );
  Path.Pident (
      Ident.create "Random"
    );
]

let rec same_path p1 p2 =
  match (p1, p2) with
  | Path.Pident id1, Path.Pident id2 ->
     Ident.equal id1 id2
  | Path.Pdot(p1, s1, _pos1), Path.Pdot(p2, s2, _pos2) ->
     s1 = s2 && same_path p1 p2
  | Path.Papply(fun1, arg1), Path.Papply(fun2, arg2) ->
     same_path fun1 fun2 && same_path arg1 arg2
  | (_, _) -> false

let path_parent path =
  let rec aux =
    function
    | Path.Pdot (Path.Pident id as p',_,_) -> p'
    | Path.Pdot (Path.Pdot (_,s,n) as p',_,_) -> Path.Pdot (aux p',s,n)
    | _ -> failwith "path is not valid"
  in
  match path with
  | Path.Pdot (Path.Pdot _,_,_) | Path.Pdot (Path.Pident _,_,_) ->
     Some (aux path)
  | _ ->
     None

module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Module Utilization"
    let version = "1"
    let short_name = "module_utilization"
    let details = "Check some properties of module utilizations"
    let enable = true
  end)

type warning =
  | IdentifierInUnrecommandedModule of string * Path.t
  | UseOpenDirective of Path.t
  | UseEnvironmentModificationModule of Path.t

let w_identifier_in_unrecommanded_mod = Linter.new_warning
    ~id:1
    ~short_name:"identifier_from_unrecommanded_module"
    ~msg:"Identifier \"$ident\" is in the unrecommanded module $mod."
    ~severity:1

let w_open_directive = Linter.new_warning
    ~id:2
    ~short_name:"use_open_directive"
    ~msg:"Avoid to use the open directive."
    ~severity:1

let w_environment_modification_mod = Linter.new_warning
    ~id:3
    ~short_name:"environment_modification_module"
    ~msg:"$mod modifies the global execution environment."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | IdentifierInUnrecommandedModule (ident, pmod) ->
	 w_identifier_in_unrecommanded_mod, [
          ("ident",ident);
          ("mod",Path.name pmod)
        ]
      | UseOpenDirective p ->
         w_open_directive, []
      | UseEnvironmentModificationModule p ->
         w_environment_modification_mod, [
          ("mod", Path.name p)
        ]
  end)

let iter =
  let module IterArg = struct
    include Typedtree_iter.DefaultIteratorArgument

    let is_unrecommanded mpath =
      List.exists (same_path mpath) unrecommanded_modules

    let is_environment_modifier mpath =
      List.exists (same_path mpath) environment_modification_modules

    let process_module_opening mpath loc =
      Warnings.report loc (UseOpenDirective mpath)

    let process_ident ident_path loc =
      match path_parent ident_path with
      | None ->
         ()
      | Some parent ->
         if is_unrecommanded parent then begin
           Warnings.report
             loc
             (IdentifierInUnrecommandedModule (Path.last ident_path, parent))
         end;
         if is_environment_modifier parent then begin
           Warnings.report
             loc
             (UseEnvironmentModificationModule parent)
         end

    let enter_expression expr =
      let open Typedtree in
      let open Asttypes in
      begin match expr.exp_desc with
      | Texp_ident (ident_path,_,_) -> process_ident ident_path expr.exp_loc
      | _ -> ()
      end;
      List.iter begin function extra,loc,_ ->
        match extra with
	| Texp_open (_,p,_,_) -> process_module_opening p loc
	| _ -> ()
      end expr.exp_extra

    let enter_pattern pat =
      let open Typedtree in
      let open Asttypes in
      List.iter
	begin function extra,loc,_ ->
	   match extra with
	   | Tpat_open (p,_,_) -> process_module_opening p loc
	   | _ -> ()
	end pat.pat_extra

    let enter_structure_item str =
      let open Typedtree in
      let open Asttypes in
      match str.str_desc with
      | Tstr_open od -> process_module_opening od.open_path od.open_loc
      | _ -> ()

    let enter_signature_item si =
      let open Typedtree in
      let open Asttypes in
      match si.sig_desc with
      | Tsig_open od -> process_module_opening od.open_path od.open_loc
      | _ -> ()

  end in
  (module IterArg : Typedtree_iter.IteratorArgument)

module MainML = Linter.MakeInputCMT(struct
    let main cmt = Typedtree_iter.iter_structure iter cmt
  end)
