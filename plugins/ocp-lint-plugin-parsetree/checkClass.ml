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
    let name = "Check Class"
    let version = "1"
    let short_name = "check_class"
    let details = "Check some properties on class"
    let enable = true
  end)

type warning =
  | ShouldUseRecord

let w_should_use_record = Linter.new_warning
    ~id:1
    ~short_name:"use_record_rather_than_class"
    ~msg:"Should use a record rather than a class."
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

    let process_class loc =
      Warnings.report loc (ShouldUseRecord)

     let enter_structure_item item =
      let open Parsetree in
      let open Asttypes in
      begin match item.pstr_desc with
      | Pstr_class _ -> process_class item.pstr_loc
      | _ -> ()
      end

  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = Linter.MakeInputStructure(struct
    let main ast = Parsetree_iter.iter_structure iter ast
  end)
