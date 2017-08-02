open SimpleConfig

type class_id =
    string

type class_inherit_info = {
  class_loc : Location.t;
  mutable class_parents : class_id list;
}

let inheritance_graph : (class_id, class_inherit_info) Hashtbl.t =
  Hashtbl.create 32

let add_inheritance loc sub super =
  let sub_id = Path.name sub in
  let super_id = Path.name super in
  let info =
    try
      Hashtbl.find inheritance_graph sub_id
    with
      Not_found ->
       let default_info =
         {
           class_loc = loc;
           class_parents = []
         }
       in
       Hashtbl.add inheritance_graph sub_id default_info;
       default_info
  in
  info.class_parents <- super_id :: info.class_parents

let class_parents clss =
  let rec aux clss =
    try
      let info = Hashtbl.find inheritance_graph clss in
      info.class_parents
      |> List.map begin fun parent ->
           List.map begin fun path ->
             clss :: path
           end (aux parent)
         end
      |> List.flatten
    with
      Not_found -> [[clss]]
  in
  aux clss

let default_inheritance_depth =
  2

module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Class Inheritance"
    let version = "1"
    let short_name = "class_inheritance"
    let details = "Check some properties of the classes inheritance"
    let enable = true
  end)

type warning =
  | DeepInheritance of string * int * int

let w_deep_inheritance = Linter.new_warning
    ~id:1
    ~short_name:"deep_inheritance"
    ~msg:"$clss have too high inheritance depth ($dep). The max is $maxdep."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | DeepInheritance (clss, depth, max_depth) ->
	 w_deep_inheritance, [
          ("clss",clss);
          ("dep", string_of_int depth);
          ("maxdep", string_of_int max_depth)
        ]
  end)

let iter =
  let module IterArg = struct
    include Typedtree_iter.DefaultIteratorArgument

    let process_class_inherit loc sub super =
      add_inheritance loc sub super

    let enter_class_declaration decl =
      let open Typedtree in
      let open Asttypes in
      let open Types in
      begin match decl.ci_expr.cl_desc with
      | Tcl_structure class_struct ->
         List.iter begin fun field ->
           begin match field.cf_desc with
           | Tcf_inherit (_, class_expr, _, _, _) ->
              begin match class_expr.cl_desc with
              | Tcl_constraint
                ({cl_desc = Tcl_ident (path_super, _, _); _}, _, _, _, _) ->
                 process_class_inherit
                   decl.ci_loc
                   decl.ci_type_decl.clty_path
                   path_super;
              | _ -> ()
              end
           | _ -> ()
           end
         end class_struct.cstr_fields
      | _ -> ()
      end

  end in
  (module IterArg : Typedtree_iter.IteratorArgument)

let max_inheritance_depth = Linter.create_option
    "max_inheritance_depth"
    "Maximum inheritance depth"
    "Maximum inheritance depth"
    SimpleConfig.int_option
    default_inheritance_depth

let check_class_inheritance clss loc max_depth =
  let inheritance = class_parents clss in
  let inheritance_depth =
    List.fold_left begin fun acc path ->
      max acc (List.length path)
    end 0 inheritance
  in
  if inheritance_depth > max_depth then begin
    Warnings.report
      loc
      (DeepInheritance (clss, inheritance_depth, max_depth))
  end

let check_inheritance max_depth =
  Hashtbl.iter begin fun clss info ->
    check_class_inheritance clss info.class_loc max_depth
  end inheritance_graph

module MainML = Linter.MakeInputCMT(struct
  let main cmt =
    Typedtree_iter.iter_structure iter cmt;
    check_inheritance !!max_inheritance_depth
  end)
