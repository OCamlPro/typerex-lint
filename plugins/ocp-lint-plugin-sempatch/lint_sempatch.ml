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

open Sempatch
open Lint_plugin_error
open Lint_warning_decl
open Lint_warning

(**************************** Sempatch utils ******************************)

type patch_source = File of string | Content of string

let read_patches =
  List.map (fun patch_source ->
      match patch_source with
      | File filename ->
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
          (raise (Plugin_error(Patch_file_not_found filename)))
      | Content cnt ->
        Patch.from_lexbuf (Lexing.from_string cnt))

let wdecls = WarningDeclaration.empty ()

let add_decl decl = WarningDeclaration.add decl wdecls

(* TODO This function should be exported in ocp-sempatch. *)
let map_args env args =
  List.map (fun str ->
      match Substitution.get str env with
      | Some ast -> (str, Ast_element.to_string ast)
      | None -> (str, "xx"))
    args

let new_warning loc lname ~decl ~args =
  let open Lint_warning_types in
  add_decl decl;
  (* TODO: cago: here we have to re-set the long help with the id and the
         short_name of the warning. It will be displayed in the config file. *)
  let msg = Lint_utils.substitute decl.message args in
  Warning.add Plugin_patch.PluginPatch.short_name lname loc decl msg

let sempatch_report lname decl matching patch =
  (* TODO Warning number can be override by the user. *)
  new_warning
    (Match.get_location matching)
    lname
    ~decl
    ~args:(map_args
             (Match.get_substitutions matching)
             (Patch.get_metavariables patch))

(******************************************************************************)

let default_patches =
  (* To add a static file, edit plugins/ocp-lint-patch/build.ocp *)
  List.map (fun (file, content) -> Content content) Global_static_files.files

let user_patches =
  try
    let path = Sys.getenv "OCPLINT_PATCHES" in
    let files = ref [] in
    Lint_utils.iter_files (fun file ->
        if Filename.check_suffix file ".md" then
          files := (File (Filename.concat path file)) :: !files)
      path;
    !files
  with Not_found -> []

let generate_version linter_version patches =
  if patches = [] then
    linter_version
  else
    let patches_file =
      List.fold_left (fun acc p ->
          match p with
          | File s -> s :: acc
          | Content _ -> acc) [] patches in
    let patches_content = List.map Lint_utils.read_file patches_file in
    let patches_content_str =
      List.fold_left (fun acc str -> acc ^ str) "" patches_content in
    Digest.to_hex (Digest.string patches_content_str)

module LintSempatch = Plugin_patch.PluginPatch.MakeLint(struct
    let name = "Lint from semantic patches."
    let version = generate_version "1" user_patches
    let short_name = "sempatch_lint"
    let details = "Lint from semantic patches."
    let enable = true
  end)

let patches =
  (read_patches default_patches) @
  (read_patches user_patches)

let iter =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument
    let enter_expression structure =
      List.iteri (fun i patches ->
          let matches =
            Patch.parallel_apply
              patches (Ast_element.from_expression structure) in
          List.iter (fun matching ->
              let patch =
                List.find
                  (fun p ->
                     Patch.get_name p = Match.get_patch_name matching)
                  patches in
              let short_name = Patch.get_name patch in
              let msg =
                match Patch.get_msg patch with
                (* TODO replace by the result of the patch. *)
                  None -> "You should use ... instead of ..."
                | Some msg -> msg in
              let severity_opt = Patch.get_field "severity" patch in
              let severity = match severity_opt with
                | None -> 1
                | Some str -> try int_of_string str with exn -> 1 in
              let decl = LintSempatch.new_warning
                  ~id:(i + 1)
                  ~short_name
                  ~msg
                  ~severity in
              sempatch_report
                LintSempatch.short_name
                decl
                matching patch)
            matches)
        patches
  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = LintSempatch.MakeInputStructure(struct
    let main ast =
      try
        Parsetree_iter.iter_structure iter ast
      with Failure.SempatchException e ->
        raise (Plugin_error (Sempatch_failure (Failure.to_string e)))
  end)
