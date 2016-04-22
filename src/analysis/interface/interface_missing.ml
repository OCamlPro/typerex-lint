open SimpleConfig (* for !! *)

(* We will register this linter to the Mascot plugin. *)
module Mascot = Plugin_mascot.PluginMascot

let details = "Missing interface."

module CodeLength = Mascot.MakeLint(struct
    let name = "Missing interface"
    let short_name = "interface-missing"
    let details = details
  end)

type warning = MissingInterface of string

module Warnings = CodeLength.MakeWarnings(struct
    type t = warning

    let missing loc args = CodeLength.new_warning
        loc
        1
        [ Warning.kind_interface ]
        ~short_name:"missing-interface"
        ~msg:"Missing interface for '%file'."
        ~args

    let report loc = function
      | MissingInterface file ->
        missing loc [("%file", file)]
  end)

let mli = ".mli"

let check source =
  let modname = Filename.chop_extension source in
  if not (Sys.file_exists (modname ^ mli)) then
    Warnings.report Location.none (MissingInterface source)

(* Registering a main entry to the linter *)
module MainSRC = CodeLength.MakeInputMLI(struct
    let main source = check source
  end)
