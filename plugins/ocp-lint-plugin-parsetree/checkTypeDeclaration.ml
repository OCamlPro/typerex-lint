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

module Linter = Plugin_parsetree.Plugin.MakeLint(struct
    let name = "Type Declaration"
    let version = "1"
    let short_name = "type_declaration"
    let details = "Check some properties on types declarations"
    let enabled = true
  end)

type warning =
  | AliasType of string

let w_alias_type = Linter.new_warning
    ~id:1
    ~short_name:"alias_type"
    ~msg:"$alias is just an alias."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | AliasType typ ->
        w_alias_type, [("alias", typ)]
  end)

let iter =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument

    let process_alias loc typ =
      Warnings.report loc (AliasType typ)

    let enter_type_declaration decl =
      let open Parsetree in
      let open Asttypes in
      match decl.ptype_kind with
      | Ptype_abstract -> process_alias decl.ptype_name.loc decl.ptype_name.txt
      | _ -> ()

  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = Linter.MakeInputStructure(struct
    let main ast = Parsetree_iter.iter_structure iter ast
  end)
