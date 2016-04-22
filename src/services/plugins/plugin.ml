open Plugin_error

module LintMap = Map.Make (String)

let plugins = Hashtbl.create 42

let register_plugin plugin =
  try
    let _ = Hashtbl.find plugins plugin in
    raise (Plugin_error(Plugin_already_registered plugin))
  with Not_found ->
    Hashtbl.add plugins plugin LintMap.empty

let register_main plugin cname main =
  try
    let lints = Hashtbl.find plugins plugin in
    try
      let runs = LintMap.find cname lints in
      let new_lints = LintMap.add cname (main :: runs) lints in
      Hashtbl.replace plugins plugin new_lints
    with Not_found ->
      Hashtbl.replace plugins plugin (LintMap.add cname [main] lints)
  with Not_found ->
    raise (Plugin_error(Plugin_not_found plugin))

let iter_plugins f =
  Hashtbl.iter (fun pname checks ->
      LintMap.iter (fun cname runs -> f pname cname runs) checks)
    plugins

let iter_plugins f =
  Hashtbl.iter (fun plugin checks -> f plugin checks) plugins

module MakePlugin(P : Plugin_types.PluginArg) = struct
  module Config = Configuration.DefaultConfig

  let name = P.name
  let short_name = P.short_name
  let details = P.details

  let warnings = Warning.empty

  module Plugin = struct
    let name = name
    let short_name = short_name
    let details = details
    let warnings = warnings
    module Config = Config
  end
  let plugin = (module Plugin : Plugin_types.PLUGIN)

  let create_option options short_help lhelp ty default =
    Config.create_option options ~short_help [lhelp] ~level:0 ty default

  module MakeLint (C : Lint.LintArg) = struct

    let name = C.name
    let short_name = C.short_name
    let details = C.details

    let new_warning loc num cats ~short_name ~msg ~args = (* TODO *)
      let msg = Utils.subsitute msg args in
      Warning.add loc num cats short_name msg warnings

    let create_option option short_help lhelp ty default =
      let option = [P.short_name; short_name; option] in
      Config.create_option option ~short_help [lhelp] ~level:0 ty default

    module MakeWarnings (WA : Warning_types.WarningArg) = struct
      type t = WA.t
      let report = WA.report
    end

    module Register(I : Input.INPUT) =
    struct
      let () = register_main plugin C.short_name (I.input)
    end

    module MakeInputStructure(S : Input.STRUCTURE) = struct
      module R = Register(struct let input = Input.InStruct S.main end)
    end

    module MakeInputInterface (I : Input.INTERFACE) = struct
      module R = Register (struct let input = Input.InInterf I.main end)
    end

    module MakeInputToplevelPhrase (T : Input.TOPLEVEL) = struct
      module R = Register (struct let input = Input.InTop T.main end)
    end

    module MakeInputCMT(C : Input.CMT) = struct
      module R = Register (struct let input = Input.InCmt C.main end)
    end

    module MakeInputML (ML : Input.ML) = struct
      module R = Register (struct let input = Input.InMl ML.main end)
    end

    module MakeInputMLI (MLI : Input.MLI) = struct
      module R = Register (struct let input = Input.InMli MLI.main end)
    end

    module MakeInputAll (All : Input.ALL) = struct
      module R = Register (struct let input = Input.InAll All.main end)
    end
  end (* MakeCheck *)

  let () =
    (* Creating 2 defaults options for plugins: "--plugin" and "--no-plugin" *)
    ignore (create_option
        ["no-"^ P.short_name]
        details
        details
        SimpleConfig.bool_option true);
    ignore (create_option
        [P.short_name]
        details
        details
        SimpleConfig.bool_option true);

    try
      register_plugin plugin
    with Plugin_error(error) ->
      failwith (Plugin_error.to_string error)
end (* MakePlugin*)
