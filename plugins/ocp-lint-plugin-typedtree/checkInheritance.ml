open SimpleConfig

let default_inheritance_depth =
  2

module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Class Inheritance"
    let version = "1"
    let short_name = "class_inheritance"
    let details = "Checks some properties of the classes inheritance"
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

let inheritance_parent_class : (string, (string * Location.t)) Hashtbl.t =
  Hashtbl.create 32

let add_inheritance loc sub super =
  Hashtbl.add inheritance_parent_class sub (super,loc)

let rec class_parents clss =
  let parents =
    try
      let parent, _ = Hashtbl.find inheritance_parent_class clss in
      class_parents parent
    with
      Not_found -> []
  in
  clss :: parents

let check_class_inheritance clss loc max_depth =
  let inheritance = class_parents clss in
  let inheritance_depth = List.length inheritance in
  if inheritance_depth > max_depth then begin
     Warnings.report loc (DeepInheritance (clss, inheritance_depth, max_depth))
  end

let check_inheritance max_depth =
  Hashtbl.iter begin fun clss (_,loc) ->
    check_class_inheritance clss loc max_depth
  end inheritance_parent_class

let iter =
  let module IterArg = struct
    include Typedtree_iter.DefaultIteratorArgument

    let process_class_inherit loc sub super =
      add_inheritance loc sub super

    let enter_class_declaration decl =
      let open Typedtree in
      let open Asttypes in
      begin match decl.ci_expr.cl_desc with
      | Tcl_structure class_struct ->
         List.iter begin fun field ->
           begin match field.cf_desc with
           | Tcf_inherit (_, class_expr, _, _, _) ->
              begin match class_expr.cl_desc with
              | Tcl_constraint
                ({cl_desc = Tcl_ident (p_super, _, _); _}, _, _, _, _) ->
                 process_class_inherit
                   decl.ci_loc
                   decl.ci_id_name.txt
                   (Path.name p_super)
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

module MainML = Linter.MakeInputCMT(struct
  let main cmt =
    Typedtree_iter.iter_structure iter cmt;
    check_inheritance !!max_inheritance_depth
  end)
