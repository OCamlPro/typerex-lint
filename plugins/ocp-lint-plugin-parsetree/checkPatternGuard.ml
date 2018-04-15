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
    let name = "Pattern Guard"
    let version = "1"
    let short_name = "pattern_guard"
    let details = "Check some properties on guards of patterns"
    let enabled = true
  end)

type warning =
  | UseGuard

let w_use_guard = Linter.new_warning
    ~id:1
    ~short_name:"use_guard"
    ~msg:"Avoid to use guard."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | UseGuard ->
        w_use_guard, []
  end)

let iter =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument

    let process_guard loc =
      Warnings.report loc UseGuard

    let process_case case =
      let open Asttypes in
      let open Parsetree in
      begin match case.pc_guard with
      | Some guard ->
         process_guard guard.pexp_loc
      | _ -> ()
      end

    let enter_expression exp =
      let open Asttypes in
      let open Parsetree in
      match exp.pexp_desc with
      | Pexp_match (_, cases)
      | Pexp_function cases
      | Pexp_try (_, cases) ->
         List.iter process_case cases
      | _ -> ()

  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = Linter.MakeInputStructure(struct
    let main ast = Parsetree_iter.iter_structure iter ast
  end)
