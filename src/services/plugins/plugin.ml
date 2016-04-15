
module type PluginArg = sig
  val name : string
  val short_name : string
  val details : string
end

type input =
    | AST of (Parsetree.structure -> unit)
    | CMT of (Cmt_format.cmt_infos -> unit)
    | SRC of (string list -> unit)

(* (plugin_name, (check_name, main)) *)
let mains : (string, (string * input list option) list) Hashtbl.t =
  Hashtbl.create 42

let register_main pname main =          (* xxx todo *)
  try
    let checks = Hashtbl.find mains pname in
    let new_runs =
      List.map (fun (check_name, runs) ->
          match runs with
          | None -> check_name, Some [main]
          | Some c -> check_name, Some (main :: c) ) checks in
    Hashtbl.replace mains pname new_runs
  with Not_found ->
    Format.eprintf "Plugin %S not found" pname;
    raise Not_found

module MakePlugin(P : PluginArg) = struct
  module Config = Configuration.DefaultConfig

  let name = P.name
  let short_name = P.short_name
  let details = P.details

  let register_check c =                (* xxx todo *)
    let module Check = (val c : Check.CheckArg) in
    try
      let checks = Hashtbl.find mains P.name in
      Hashtbl.replace mains P.name ((Check.name, None) :: checks)
    with Not_found ->
      Hashtbl.add mains P.name [(Check.name, None)]

  let warnings = ()
  let report_warning () = assert false

  let create_option options details ty default = assert false

  module MakeCheck(Chk : Check.CheckArg) = struct

    let name = Chk.name
    let short_name = Chk.short_name
    let details = Chk.details

    let create_option option short_help lhelp level ty default =
      let option = [P.name; option] in
      Config.create_option option ~short_help [lhelp] ~level ty default

    let report warning =
      report_warning ()

    module MakeWarnings (WA : Warning.WarningArg) = struct
      type t = WA.t
      let report = report
      let warnings () = []
    end

  let () =
      let check = (module Chk : Check.CheckArg) in
     register_check check
  end


  module MakeInputAST (AST : Input.InputAST) = struct
    let main = AST.main
    let () =
      register_main P.name (AST main)
  end

  module MakeInputCMT (CMT : Input.InputCMT) = struct
    let main = CMT.main
    let () =
      register_main P.name (CMT main)
  end

  module MakeInputSRC (SRC : Input.InputSRC) = struct
    let main = SRC.main
    let () =
      register_main P.name (SRC main)
  end


end
