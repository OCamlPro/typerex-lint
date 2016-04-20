open Plugin_error

module LintMap = Map.Make (String)

let plugins : (string, (Input.input list) LintMap.t) Hashtbl.t =
  Hashtbl.create 42

let register_plugin pname =
  try
    let _ = Hashtbl.find plugins pname in
    raise (Plugin_error(Plugin_already_registered pname))
  with Not_found ->
    Hashtbl.add plugins pname LintMap.empty

let register_main pname cname main =
  try
    let lints = Hashtbl.find plugins pname in
    try
      let runs = LintMap.find cname lints in
      let new_lints = LintMap.add cname (main :: runs) lints in
      Hashtbl.replace plugins pname new_lints
    with Not_found ->
      Hashtbl.replace plugins pname (LintMap.add cname [main] LintMap.empty)
  with Not_found ->
    raise (Plugin_error(Plugin_not_found pname))

let iter_plugins f  =
  Hashtbl.iter (fun pname checks ->
      LintMap.iter (fun cname runs -> f pname cname runs) checks)
    plugins

module MakePlugin(P : Plugin_types.PluginArg) = struct
  module Config = Configuration.DefaultConfig

  let name = P.name
  let short_name = P.short_name
  let details = P.details

  let warnings = Warning.empty

  let create_option option short_help lhelp level ty default =
    let option = [option] in
    Config.create_option option ~short_help [lhelp] ~level ty default

  module MakeCheck(C : Check.CheckArg) = struct

    let name = C.name
    let short_name = C.short_name
    let details = C.details

    let new_warning loc num cats ~short_name ~msg ~args = (* TODO *)
      let msg = Utils.subsitute msg args in
      Warning.add loc num cats short_name msg warnings

    let create_option option short_help lhelp ty default =
      let option = [P.name; option] in
      Config.create_option option ~short_help [lhelp] ~level:0 ty default

    module MakeWarnings (WA : Warning_types.WarningArg) = struct
      type t = WA.t
      let report = WA.report
    end

    module MakeInputAST (AST : Input.InputAST) = struct
      let main = AST.main
      let () = register_main P.short_name C.short_name (Input.AST main)
    end

    module MakeInputCMT (CMT : Input.InputCMT) = struct
      let main = CMT.main
      let () = register_main P.short_name C.short_name (Input.CMT main)
    end

    module MakeInputSRC (SRC : Input.InputSRC) = struct
      let main = SRC.main
      let () = register_main P.short_name  C.short_name (Input.SRC main)
    end
  end (* MakeCheck *)

  let () =
    try register_plugin P.short_name
    with Plugin_error(error) ->
      failwith (Plugin_error.to_string error)
end (* MakePlugin*)
