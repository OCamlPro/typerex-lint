open SimpleConfig (* for !! *)

(* We will register this linter to the Mascot plugin. *)
module Mascot = Plugin_mascot.PluginMascot

let details =
  Printf.sprintf
    "Checks that every identifier has a minimum and a maximum length. \
     Usually, short names implies that the code is harder to read and \
     understand."

module CodeIdentifierLength = Mascot.MakeLint(struct
    let name = "Code Identifier Length"
    let short_name = "code_identifier_length"
    let details = details
  end)

(* Defining/Using option from configuration file / command line *)
let min_identifier_length = CodeIdentifierLength.create_option
    "min_identifier_length"
    "Identifiers with a shorter name will trigger a warning"
    "Identifiers with a shorter name will trigger a warning"
    SimpleConfig.int_option
    2


let max_identifier_length = CodeIdentifierLength.create_option
     "max_identifier_length"
    "Identifiers with a longer name will trigger a warning"
    "Identifiers with a longer name will trigger a warning"
    SimpleConfig.int_option 30

type warnings = Short of string | Long of string

module Warnings = CodeIdentifierLength.MakeWarnings(struct
    type t = warnings

    let w_too_short loc args = CodeIdentifierLength.new_warning
        loc
        1
        [ Warning.kind_code ]
        ~short_name:"identifier_too_short"
        ~msg:"%id is too short: it should be at least of size '%size'."
        ~args

    let w_too_long loc args = CodeIdentifierLength.new_warning
        loc
        2
        [ Warning.kind_code ]
        ~short_name:"identifier_too_long"
        ~msg:"%id is too long: it should be at most of size '%size'."
        ~args

    let report loc = function
      | Short id ->
        w_too_short
          loc
          [("%id", id); ("%size", string_of_int !!min_identifier_length)]
      | Long id ->
        w_too_long
          loc
          [("%id", id); ("%size", string_of_int !!max_identifier_length)]
  end)

let iter =
  let module IterArg = struct
    include ParsetreeIter.DefaultIteratorArgument

    let enter_pattern pat =
      let open Parsetree in
      let open Asttypes in
      begin match pat.ppat_desc with
        | Ppat_var ident ->
          let id_str = ident.txt in
          let id_loc = ident.loc in
          let id_len = String.length id_str in
          if id_len < !!min_identifier_length then
            Warnings.report id_loc (Short id_str);
          if id_len > !!max_identifier_length then
            Warnings.report id_loc (Long id_str)
        | _ -> ()
      end
  end in
  (module IterArg : ParsetreeIter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = CodeIdentifierLength.MakeInputStructure(struct
    let main ast = ParsetreeIter.iter_structure iter ast
  end)
