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
    let name = "External Code"
    let version = "1"
    let short_name = "external_code"
    let details = "Check if external code is used"
    let enable = true
  end)

type warning =
  | ExternalValue of string

let w_external_value = Linter.new_warning
    ~id:1
    ~short_name:"external_value"
    ~msg:"Value \"$value\" is external defined."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | ExternalValue value ->
        w_external_value, ["value", value]
  end)

let iter =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument

    let process_external_value loc value =
      Warnings.report loc (ExternalValue value)

     let enter_value_description desc =
      let open Parsetree in
      let open Asttypes in
      begin match desc.pval_prim with
      | [] -> ()
      | _ -> process_external_value desc.pval_name.loc desc.pval_name.txt
      end

  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = Linter.MakeInputStructure(struct
    let main ast = Parsetree_iter.iter_structure iter ast
  end)
