module Linter = Plugin_typedtree.Plugin.MakeLint(struct
    let name = "Identifier Masking"
    let version = "1"
    let short_name = "identifier_masking"
    let details = "Check if some identifiers are masked"
    let enabled = true
  end)

type warning =
  | GlobalMasking

let w_global_masking = Linter.new_warning
    ~id:1
    ~short_name:"global_masking"
    ~msg:"The identifier ... is masking a global identifier."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | GlobalMasking ->
	 w_global_masking, []
end)

let iter =
  let module IterArg = struct
    include Typedtree_iter.DefaultIteratorArgument

    let enter_structure_item item =
      let open Typedtree in
      let open Asttypes in
      begin match item.str_desc with
      | Tstr_open desc ->
         Env.fold_values begin fun x y _ _ ->
           ()
         end None item.str_env ()
      | _ -> ()
      end

  end in
  (module IterArg : Typedtree_iter.IteratorArgument)

module MainML = Linter.MakeInputCMT(struct
    let main cmt = Typedtree_iter.iter_structure iter cmt
  end)
