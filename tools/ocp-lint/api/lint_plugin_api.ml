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
open Lint_warning
open Lint_warning_decl

let register_plugin plugin =
  if Lint_plugin.check_uniqueness Lint_globals.plugins plugin then
    Lint_plugin.add Lint_globals.plugins plugin Lint_map.empty
  else raise (Plugin_error(Plugin_already_registered plugin))

let register_main plugin cname new_lint =
  try
    let lints = Lint_plugin.find Lint_globals.plugins plugin in
    try
      let lint = Lint_map.find cname lints in
      let module Old_lint = (val lint : Lint_types.LINT) in
      let module New_lint = (val new_lint : Lint_types.LINT) in
      let module Merge = struct
        let name = New_lint.name
        let version = New_lint.version
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

let check_lint_uniqueness plugins plugin short_name =
  let lints = Lint_plugin.find Lint_globals.plugins plugin in
  let res = ref true in
  Lint_map.iter (fun lname _ ->
      if lname = short_name then res := false)
    lints;
  !res

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
    create_option [P.short_name; lint_short_name; "ignore"]
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

  let new_warning id ~short_name ~msg ~severity =
    let open Lint_warning_types in
    let warning_decl = { short_name; message = msg; id; severity } in
    warning_decl

  module MakeLint (C : Lint_types.LINTARG) = struct

    let name = C.name
    let version = C.version
    let short_name = C.short_name
    let details = C.details
    let enable = C.enable
    let wdecls = WarningDeclaration.empty ()
    let is_unique = check_lint_uniqueness Lint_globals.plugins plugin C.short_name

    let create_option option short_help lhelp ty default =
      let option = [P.short_name; C.short_name; option] in
      Lint_globals.Config.create_option option short_help lhelp 0 ty default

    let new_warning ~id ~short_name ~msg ~severity  =
      let open Lint_warning_types in
      let decl = new_warning id ~short_name ~msg ~severity in
      WarningDeclaration.add decl wdecls;
      decl

    (** [MakeWarnings] is a functor which allows to register the warnings
        automatically in the global data structure [plugins] with the
        associated plugin and linter. *)
    module MakeWarnings (WA : Lint_warning_types.WARNINGARG) = struct
      type t = WA.t

      let report loc t =
        let open Lint_warning_types in
        let decl, args = WA.to_warning t in
        let msg = Lint_utils.substitute decl.message args in
        Warning.add P.short_name C.short_name loc decl msg

      let report_file filename t =
        let open Lint_warning_types in
        let decl, args = WA.to_warning t in
        let msg = Lint_utils.substitute decl.message args in
        let loc = Location.in_file filename in
        Warning.add P.short_name C.short_name loc decl msg

      let report_file_line file lnum w =
        let open Location in
        let open Lexing in
        let pos = { Lexing.dummy_pos with pos_fname = file; pos_lnum = lnum } in
        let loc = { Location.none with loc_start = pos; loc_end = pos} in
        report loc w

      let report_file_line_col file lnum cnum w =
        let open Location in
        let open Lexing in
        let pos = { Lexing.dummy_pos with
                    pos_fname = file;
                    pos_lnum = lnum;
                    pos_cnum = cnum;
                  } in
        let loc = { Location.none with loc_start = pos; loc_end = pos} in
        report loc w

    end

    module Register(I : Lint_input.INPUT) =
    struct
      let () =
        if not is_unique then
          raise (Plugin_error(Linter_already_registered short_name))
        else
          let module Lint = struct
            let name = name
            let version = version
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

    module MakeInputTokens (I : Lint_input.TOKENS) = struct
      module R = Register (struct
          let input = Lint_input.InTokens (wrap_plugin_exn I.main) end)
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
      register_plugin plugin
end (* MakePlugin*)
