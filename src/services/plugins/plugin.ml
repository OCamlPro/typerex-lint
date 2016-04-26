(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

open Plugin_error

let register_plugin plugin =
  try
    let _ = Hashtbl.find Globals.plugins plugin in
    raise (Plugin_error(Plugin_already_registered plugin))
  with Not_found ->
    Hashtbl.add Globals.plugins plugin Globals.LintMap.empty

let register_main plugin cname main =
  try
    let lints = Hashtbl.find Globals.plugins plugin in
    try
      let runs = Globals.LintMap.find cname lints in
      let new_lints = Globals.LintMap.add cname (main :: runs) lints in
      Hashtbl.replace Globals.plugins plugin new_lints
    with Not_found ->
      Hashtbl.replace
        Globals.plugins plugin (Globals.LintMap.add cname [main] lints)
  with Not_found ->
    raise (Plugin_error(Plugin_not_found plugin))

let iter_plugins apply =
  Hashtbl.iter (fun plugin checks -> apply plugin checks) Globals.plugins

module MakePlugin(P : Plugin_types.PluginArg) = struct

  let name = P.name
  let short_name = P.short_name
  let details = P.details

  let warnings = Warning.empty

  module Plugin = struct
    let name = name
    let short_name = short_name
    let details = details
    let warnings = warnings
  end
  let plugin = (module Plugin : Plugin_types.PLUGIN)

  let create_option options short_help lhelp ty default =
    Globals.Config.create_option options ~short_help [lhelp] ~level:0 ty default

  module MakeLint (C : Lint.LintArg) = struct

    let name = C.name
    let short_name = C.short_name
    let details = C.details

    let new_warning loc num cats ~short_name ~msg ~args = (* TODO *)
      let msg = Utils.subsitute msg args in
      Warning.add loc num cats short_name msg warnings

    let create_option option short_help lhelp ty default =
      let option = [P.short_name; short_name; option] in
      Globals.Config.create_option
        option ~short_help [lhelp] ~level:0 ty default

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
    (* Creating default options for plugins: "--plugin.enable" *)
    ignore (create_option
        [P.short_name; "enable"]
        details
        details
        SimpleConfig.bool_option true);

    try
      register_plugin plugin
    with Plugin_error(error) ->
      failwith (Plugin_error.to_string error)
end (* MakePlugin*)
