let rec same_path p1 p2 =
  match (p1, p2) with
  | Path.Pident id1, Path.Pident id2 ->
     Ident.equal id1 id2
  | Path.Pdot(p1, s1, _pos1), Path.Pdot(p2, s2, _pos2) ->
     s1 = s2 && same_path p1 p2
  | Path.Papply(fun1, arg1), Path.Papply(fun2, arg2) ->
     same_path fun1 fun2 && same_path arg1 arg2
  | (_, _) -> false

module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Check Hash Table"
    let version = "1"
    let short_name = "check_hash_table"
    let details = "Check some properties on hash table"
    let enable = true
  end)

type warning =
  | RandomizationDisable

let w_randomization_disable = Linter.new_warning
    ~id:1
    ~short_name:"randomization_disable"
    ~msg:"Hash tables should be randomized."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | RandomizationDisable ->
	 w_randomization_disable, []
  end)

let iter =
  let module IterArg = struct
    include Typedtree_iter.DefaultIteratorArgument

    let hashtbl_create =
      Path.Pdot (
          Path.Pident (
              Ident.create "Hashtbl"
            ), "create", 1
        )

    let apply_args_stack = ref []

    let is_enable_option opt =
      let open Typedtree in
      let open Asttypes in
      let open Types in
      match opt with
      | Texp_construct (_, {cstr_name = "Some"; _}, [
          {exp_desc = Texp_construct (_, {cstr_name = "true"; _}, []); _}
        ]) ->
         true
      | _ ->
         false

    let process_hashtbl_create loc =
      let open Typedtree in
      let open Asttypes in
      let open Types in
      match !apply_args_stack with
      | [args] ->
         if not (
           List.exists begin function
             | Optional "random", Some {exp_desc = opt; _} ->
                is_enable_option opt
             | _ ->
                false
           end args
         ) then
           Warnings.report
             loc
             RandomizationDisable
      | _ ->
         Warnings.report
           loc
           RandomizationDisable

    let enter_expression expr =
      let open Typedtree in
      let open Asttypes in
      begin match expr.exp_desc with
      | Texp_ident (ident_path,_,_) when same_path ident_path hashtbl_create ->
         process_hashtbl_create expr.exp_loc
      | Texp_apply (_, args) ->
          apply_args_stack :=  args :: !apply_args_stack
      | _ -> ()
      end

    let exit_expression expr =
      let open Typedtree in
      let open Asttypes in
      begin match expr.exp_desc with
      | Texp_apply (_, args) ->
         apply_args_stack := List.tl (!apply_args_stack)
      | _ -> ()
      end

  end in
  (module IterArg : Typedtree_iter.IteratorArgument)

module MainML = Linter.MakeInputCMT(struct
    let main cmt = Typedtree_iter.iter_structure iter cmt
  end)
