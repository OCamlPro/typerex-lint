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

open SimpleConfig (* for !! *)
open Plugin_complex

let details = "Checks if the module type name is capitalized."

module ModuleTypeName = PluginComplex.MakeLint(struct
    let name = "Checks on module type name."
    let short_name = "interface-module-type-name"
    let details = details
  end)

type warning = Module_name of (string * string)

let w_name = ModuleTypeName.new_warning
    [ Lint_warning.kind_code; Lint_warning.kind_interface ]
    ~short_name:"interface-module-type-name-check"
    ~msg:"Module type name '$badmodname' should be uppercase as '$goodmodname'."

module Warnings = struct
  let w_name = ModuleTypeName.instanciate w_name

  let report loc = function
    | Module_name (bad, good) ->
      w_name loc [("badmodname", bad); ("goodmodname", good)]
end

let check_module_name loc modname =
  if String.uppercase modname <> modname  then
    Warnings.report loc (Module_name (modname, String.uppercase modname))

let iter =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument

    let enter_module_type_declaration modtype =
      let open Parsetree in
      let open Asttypes in
      check_module_name modtype.pmtd_loc modtype.pmtd_name.txt
  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainMLI = ModuleTypeName.MakeInputInterface (struct
    let main ast = Parsetree_iter.iter_signature iter ast
  end)

module MainML = ModuleTypeName.MakeInputStructure(struct
    let main ast = Parsetree_iter.iter_structure iter ast
  end)
