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
    let name = "Check Tuple"
    let version = "1"
    let short_name = "check_tuple"
    let details = "Check some properties on tuple"
    let enable = true
  end)

type warning =
  | ShouldUseRecord

let w_should_use_record = Linter.new_warning
    ~id:1
    ~short_name:"use_record_rather_than_tuple"
    ~msg:"Should use a record rather than a tuple."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | ShouldUseRecord ->
         w_should_use_record, []
  end)

let iter =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument

    let process_tuple loc =
      Warnings.report loc (ShouldUseRecord)

     let enter_core_type core_type =
      let open Parsetree in
      let open Asttypes in
      begin match core_type.ptyp_desc with
      | Ptyp_tuple _ -> process_tuple core_type.ptyp_loc
      | _ -> ()
      end

    let enter_pattern pat =
      let open Parsetree in
      let open Asttypes in
      begin match pat.ppat_desc with
      | Ppat_tuple _ -> process_tuple pat.ppat_loc
      | _ -> ()
      end

     let enter_expression expr =
      let open Parsetree in
      let open Asttypes in
      begin match expr.pexp_desc with
      | Pexp_tuple _ -> process_tuple expr.pexp_loc
      | _ -> ()
      end

     let enter_constructor_declaration decl =
      let open Parsetree in
      let open Asttypes in
      begin match decl.pcd_args with
      | Pcstr_tuple _ -> process_tuple decl.pcd_loc
      | _ -> ()
      end

  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = Linter.MakeInputStructure(struct
    let main ast = Parsetree_iter.iter_structure iter ast
  end)
