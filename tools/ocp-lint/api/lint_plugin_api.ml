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

open Lint_plugin_error
open Sempatch
open Lint_warning
open Lint_warning_decl

let register_plugin plugin =
  try
    let _ = Lint_plugin.find Lint_globals.plugins plugin in
    raise (Plugin_error(Plugin_already_registered plugin))
  with Not_found ->
    Lint_plugin.add Lint_globals.plugins plugin Lint_map.empty

let register_main plugin cname new_lint =
  try
    let lints = Lint_plugin.find Lint_globals.plugins plugin in
    try
      let lint = Lint_map.find cname lints in
      let module Old_lint = (val lint : Lint_types.LINT) in
      let module New_lint = (val new_lint : Lint_types.LINT) in
      let module Merge = struct
        let name = New_lint.name
        let short_name = New_lint.short_name
        let details = New_lint.details
        let enable = New_lint.enable
        let inputs = Old_lint.inputs @ New_lint.inputs
        let wdecls = WarningDeclaration.union Old_lint.wdecls New_lint.wdecls
      end in
      let new_lints =
        Lint_map.add cname (module Merge : Lint_types.LINT) lints in
      Lint_plugin.add Lint_globals.plugins plugin new_lints
    with Not_found ->
      Lint_plugin.add
        Lint_globals.plugins plugin (Lint_map.add cname new_lint lints)
  with Not_found ->
    raise (Plugin_error(Plugin_not_found plugin))

module MakePlugin(P : Lint_plugin_types.PLUGINARG) = struct

  let name = P.name
  let short_name = P.short_name
  let details = P.details
  let enable = P.enable

  module Plugin = struct
    let name = name
    let short_name = short_name
    let details = details
    let enable = enable
  end
  let plugin = (module Plugin : Lint_plugin_types.PLUGIN)

  let create_option options short_help lhelp ty default =
    Lint_globals.Config.create_option options short_help lhelp 0 ty default

  let create_default_lint_option lint_short_name lint_long_name enable =
    let details = Printf.sprintf "Enable/Disable linter %S." lint_long_name in
    ignore @@
    create_option [P.short_name; lint_short_name; "enabled"]
      details
      details
      SimpleConfig.enable_option
      enable;
    let details =
      Printf.sprintf "Module to ignore durint the lint of %S" lint_long_name in
    ignore @@
    create_option [P.short_name; lint_short_name; "ignored_files"]
      details
      details
      (SimpleConfig.list_option SimpleConfig.string_option)
      [];
    let details =
      Printf.sprintf "Enable/Disable warnings from %S" lint_long_name in
    ignore @@
    create_option [P.short_name; lint_short_name; "warnings"]
      details
      details
      SimpleConfig.string_option
      "+A"

  let new_warning kinds ~short_name ~msg = (* TODO *)
    let open Lint_warning_types in
    let warning_decl = { kinds; short_name; message = msg } in
    warning_decl

  module MakeLintPatch (C : Lint_types.LINTPATCHARG) = struct

    let name = C.name
    let short_name = C.short_name
    let details = C.details
    let patches = C.patches
    let enable = C.enable

    let wdecls = WarningDeclaration.empty ()

    let create_option option short_help lhelp ty default =
      let option = [P.short_name; C.short_name; option] in
      Lint_globals.Config.create_option option short_help lhelp 0 ty default

    let new_warning loc warn_id kinds ~short_name ~msg ~args = (* TODO *)
      let open Lint_warning_types in
      let decl = new_warning kinds ~short_name ~msg in
      WarningDeclaration.add decl wdecls;
      (* TODO: cago: here we have to re-set the long help with the id and the
         short_name of the warning. It will be displayed in the config file. *)
      let msg = Lint_utils.substitute decl.message args in
      Warning.add P.short_name C.short_name loc warn_id decl msg

    (* TODO This function should be exported in ocp-sempatch. *)
    let map_args env args =
      List.map (fun str ->
          match Substitution.get str env with
          | Some ast -> (str, Ast_element.to_string ast)
          | None -> (str, "xx"))
        args

    module Warnings = struct
      let report warn_id matching kinds patch =
        let msg =
          match Patch.get_msg patch with
          (* TODO replace by the result of the patch. *)
            None -> "You should use ... instead of ..."
          | Some msg -> msg in
        (* TODO Warning number can be override by the user. *)
        new_warning (Match.get_location matching) warn_id kinds
          ~short_name:(Patch.get_name patch)
          ~msg
          ~args:(map_args
                   (Match.get_substitutions matching)
                   (Patch.get_metavariables patch))
    end

    let patches =
      List.map (fun filename ->
          if Sys.file_exists filename then
            try
              let ic = open_in filename in
              let patches = Patch.from_channel ic in
              close_in ic;
              patches
            with
              Failure.SempatchException e ->
              Printf.eprintf "Sempatch failure : %s\n" (Failure.to_string e);
              []
          else
            (raise (Plugin_error(Patch_file_not_found filename))))
        C.patches

    let iter =
      let module IterArg = struct
        include Parsetree_iter.DefaultIteratorArgument
        let enter_structure structure =
          List.iteri (fun i patches ->
              let matches =
                Patch.parallel_apply
                  patches (Ast_element.from_structure structure) in
              List.iter (fun matching ->
                  let patch =
                    List.find
                      (fun p ->
                         Patch.get_name p = Match.get_patch_name matching)
                      patches in
                  Warnings.report (i + 1) matching [kind_code] patch)
                matches)
            patches
      end in
      (module IterArg : Parsetree_iter.IteratorArgument)

    let () =
      let module Lint = struct
        let name = name
        let short_name = short_name
        let details = details
        let enable = enable
        let inputs = [Lint_input.InStruct (Parsetree_iter.iter_structure iter)]
        let wdecls = wdecls
      end in
      let lint = (module Lint : Lint_types.LINT) in
      register_main plugin C.short_name lint;
      create_default_lint_option C.short_name C.name C.enable
  end (* MakeLintPatch *)

  module MakeLint (C : Lint_types.LINTARG) = struct

    let name = C.name
    let short_name = C.short_name
    let details = C.details
    let enable = C.enable
    let wdecls = WarningDeclaration.empty ()

    let create_option option short_help lhelp ty default =
      let option = [P.short_name; C.short_name; option] in
      Lint_globals.Config.create_option option short_help lhelp 0 ty default

    let fresh_id =
      let cpt = ref 0 in
      fun () -> incr cpt; !cpt

    let instanciate decl =
      let open Lint_warning_types in
      let id = fresh_id () in
      (* TODO: cago: here we have to re-set the long help with the id and the
         short_name of the warning. It will be displayed in the config file. *)
      fun loc ~args ->
        let msg = Lint_utils.substitute decl.message args in
        Warning.add P.short_name C.short_name loc id decl msg

    let new_warning kinds ~short_name ~msg = (* TODO *)
      let decl = new_warning kinds ~short_name ~msg in
      WarningDeclaration.add decl wdecls;
      decl

    module Register(I : Lint_input.INPUT) =
    struct
      let () =
        let module Lint = struct
          let name = name
          let short_name = short_name
          let details = details
          let enable = enable
          let inputs = [ I.input ]
          let wdecls = wdecls
        end in
        let lint = (module Lint : Lint_types.LINT) in
        register_main plugin C.short_name lint
    end

    let wrap_plugin_exn f =
      function arg ->
      try
        ( f arg : unit )
      with
      | exn -> raise (Lint_plugin_error.Plugin_error (Plugin_exception exn))

    module MakeInputStructure(S : Lint_input.STRUCTURE) = struct
      module R = Register(struct
          let input = Lint_input.InStruct (wrap_plugin_exn S.main) end)
    end

    module MakeInputInterface (I : Lint_input.INTERFACE) = struct
      module R = Register (struct
          let input = Lint_input.InInterf (wrap_plugin_exn I.main) end)
    end

    module MakeInputToplevelPhrase (T : Lint_input.TOPLEVEL) = struct
      module R = Register (struct
          let input = Lint_input.InTop (wrap_plugin_exn T.main) end)
    end

    module MakeInputCMT(C : Lint_input.CMT) = struct
      module R = Register (struct
          let input = Lint_input.InCmt (wrap_plugin_exn C.main) end)
    end

    module MakeInputML (ML : Lint_input.ML) = struct
      module R = Register (struct
          let input = Lint_input.InMl (wrap_plugin_exn ML.main) end)
    end

    module MakeInputMLI (MLI : Lint_input.MLI) = struct
      module R = Register (struct
          let input = Lint_input.InMli (wrap_plugin_exn MLI.main) end)
    end

    module MakeInputAll (All : Lint_input.ALL) = struct
      module R = Register (struct
          let input = Lint_input.InAll (wrap_plugin_exn All.main) end)
    end
    let () =
      create_default_lint_option C.short_name C.name C.enable
  end (* MakeLint *)

  let () =
    (* Creating default options for plugins: "--plugin.enable" *)
    ignore @@
    create_option
      [P.short_name; "enabled"]
      details
      details
      SimpleConfig.enable_option P.enable;
    try
      register_plugin plugin
    with Plugin_error(error) ->
      failwith (Lint_plugin_error.to_string error)
end (* MakePlugin*)
