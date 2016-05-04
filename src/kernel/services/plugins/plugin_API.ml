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
open Sempatch

let register_plugin plugin =
  try
    let _ = Plugin.find Globals.plugins plugin in
    raise (Plugin_error(Plugin_already_registered plugin))
  with Not_found ->
    Plugin.add Globals.plugins plugin Globals.LintMap.empty

let register_main plugin cname main =
  try
    let lints = Plugin.find Globals.plugins plugin in
    try
      let runs = Globals.LintMap.find cname lints in
      let new_lints = Globals.LintMap.add cname (main :: runs) lints in
      Plugin.add Globals.plugins plugin new_lints
    with Not_found ->
      Plugin.add
        Globals.plugins plugin (Globals.LintMap.add cname [main] lints)
  with Not_found ->
    raise (Plugin_error(Plugin_not_found plugin))

module MakePlugin(P : Plugin_types.PluginArg) = struct

  let name = P.name
  let short_name = P.short_name
  let details = P.details

  let warnings = Warning.empty ()

  module Plugin = struct
    let name = name
    let short_name = short_name
    let details = details
    let warnings = warnings
  end
  let plugin = (module Plugin : Plugin_types.PLUGIN)

  let create_option options short_help lhelp ty default =
    Globals.Config.create_option options ~short_help [lhelp] ~level:0 ty default

  module MakeLintPatch (C : Lint.LintPatchArg) = struct

    let name = C.name
    let short_name = C.short_name
    let details = C.details
    let patches = C.patches

    let new_warning loc num cats ~short_name ~msg ~args = (* TODO *)
      let msg = Utils.subsitute msg args in
      Warning.add loc num cats short_name msg Plugin.warnings

    (* TODO This function should be exported in ocp-sempatch. *)
    let map_args env args =
      List.map (fun str ->
            match Substitution.get str env with
            | Some ast -> (str, Ast_element.to_string ast)
            | None -> (str, "xx"))
        args

    let report matching kinds patch =
      let msg =
        match Patch.get_msg patch with
        (* TODO replace by the result of the patch. *)
          None -> "You should use ... instead of ..."
        | Some msg -> msg in
      (* TODO Warning number can be override by the user. *)
      new_warning (Match.get_location matching) 1 kinds
        ~short_name:(Patch.get_name patch)
        ~msg
        ~args:(map_args
                 (Match.get_substitutions matching)
                 (Patch.get_metavariables patch))

    let iter =
      let module IterArg = struct
        include ParsetreeIter.DefaultIteratorArgument
        let enter_expression expr =
          List.iter (fun filename ->
              if Sys.file_exists filename then begin
                let ic = open_in filename in
                let patches = Patch.from_channel ic in
                let matches =
                  Patch.parallel_apply patches (Ast_element.Expression expr) in
                List.iter (fun matching ->
                    let patch =
                      List.find
                        (fun p ->
                           Patch.get_name p = Match.get_patch_name matching)
                        patches in
                    report matching [Warning.kind_code] patch)
                  matches
              end else
                raise (Plugin_error(Patch_file_not_found filename)))
            patches
      end in
      (module IterArg : ParsetreeIter.IteratorArgument)

    let () =
      let input = Input.InStruct (ParsetreeIter.iter_structure iter) in
      register_main plugin C.short_name (input)
  end

  module MakeLint (C : Lint.LintArg) = struct

    let name = C.name
    let short_name = C.short_name
    let details = C.details

    let new_warning loc num cats ~short_name ~msg ~args = (* TODO *)
      let msg = Utils.subsitute msg args in
      Warning.add loc num cats short_name msg Plugin.warnings

    let create_option option short_help lhelp ty default =
      let option = [P.short_name; short_name; option] in
      Globals.Config.create_option option ~short_help [lhelp] ~level:0 ty default

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
        [P.short_name; "disable"]
        details
        details
        SimpleConfig.flag_option false);

    try
      register_plugin plugin
    with Plugin_error(error) ->
      failwith (Plugin_error.to_string error)
end (* MakePlugin*)
