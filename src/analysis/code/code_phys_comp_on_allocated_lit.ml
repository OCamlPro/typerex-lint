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

module Core = Plugin_core.PluginCore

let details =
  "Checks physical comparison between two allocated litterals."

module PhysCompLit = Core.MakeLint(struct
    let name = "Physical comparison between allocated litterals."
    let short_name = "phys-comp-allocated-lit"
    let details = details
  end)

type warning =
  | String
  | Tuple
  | Construct_some
  | Variant_some
  | Arr
  | Rec
  | Not_lit

let to_string ty1 ty2 =
  match ty1, ty2 with
  | String, String -> "Strings"
  | Tuple, Tuple -> "Tuples"
  | Construct_some, Construct_some -> "Some constructors"
  | Variant_some, Variant_some -> "Some variants"
  | Arr, Arr -> "Arrays"
  | Rec, Rec -> "Records"
  | Not_lit, Not_lit -> "Not a litteral"
  | _ -> "Allocated litterals"

let w_phys_comp = PhysCompLit.new_warning
    [ Warning.kind_code ]
    ~short_name:"phys-comp-allocated-lit-checks"
    ~msg:"Physical comparison on '$lit'."

module Warnings = struct
  let w_phys_comp = PhysCompLit.instanciate w_phys_comp

  let report loc ty1 ty2 =
    let ty = to_string ty1 ty2 in
    w_phys_comp loc [("lit", ty)]

end

(** Allocated litterals *)
let is_allocated_lit expr =
  let open Asttypes in
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_constant (Const_string _) ->  true, String
  | Pexp_tuple _ -> true, Tuple
  | Pexp_construct (_, Some _) -> true, Construct_some
  | Pexp_variant (_, Some _) -> true, Variant_some
  | Pexp_array _ -> true, Arr
  | Pexp_record _ -> true, Rec
  | _ -> false, Not_lit

let is_physical_comp expr =
  let open Asttypes in
  let open Parsetree in
  match expr.pexp_desc with
  | Pexp_ident id ->
    let flat = Longident.flatten id.txt in
    let str_fun = String.concat "." flat in
    if str_fun = "==" || str_fun = "!=" then true else false
  | _ -> false

let iter =
  let module IterArg = struct
    include ParsetreeIter.DefaultIteratorArgument
    let enter_expression expr =
      let open Asttypes in
      let open Parsetree in
      match expr.pexp_desc with
      | Pexp_apply (f, args) ->
        if is_physical_comp f then
          begin
            match args with
            | [(_, expr1); (_, expr2)] ->
              let flag1, ty1 = is_allocated_lit expr1 in
              let flag2, ty2 = is_allocated_lit expr2 in
              if flag1 && flag2 then
                Warnings.report expr.pexp_loc ty1 ty2
            | _ -> ()
          end
      | _ -> ()
  end in
  (module IterArg : ParsetreeIter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = PhysCompLit.MakeInputStructure(struct
    let main ast = ParsetreeIter.iter_structure iter ast
  end)
