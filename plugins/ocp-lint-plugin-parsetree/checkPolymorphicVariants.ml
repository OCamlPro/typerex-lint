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
    let name = "Polymorphic Variants"
    let version = "1"
    let short_name = "polymorphic_variants"
    let details = "Check if polymorphics variants are used"
    let enable = true
  end)

type warning =
  | PolymorphicVariantType

let w_polymorphic_variants_type = Linter.new_warning
    ~id:1
    ~short_name:"polymorphics_variants_type"
    ~msg:"Should not use polymorphics variants type."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | PolymorphicVariantType ->
        w_polymorphic_variants_type, []
  end)

let iter =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument

    let process_poly_variant loc =
      Warnings.report loc PolymorphicVariantType

    let enter_core_type core_type =
      let open Parsetree in
      let open Asttypes in
      match core_type.ptyp_desc with
      | Ptyp_variant _ -> process_poly_variant core_type.ptyp_loc
      | _ -> ()

    let enter_pattern pat =
      let open Parsetree in
      let open Asttypes in
      begin match pat.ppat_desc with
      | Ppat_variant _ -> process_poly_variant pat.ppat_loc
      | _ -> ()
      end

     let enter_expression expr =
      let open Parsetree in
      let open Asttypes in
      begin match expr.pexp_desc with
      | Pexp_variant _ -> process_poly_variant expr.pexp_loc
      | _ -> ()
      end

  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = Linter.MakeInputStructure(struct
    let main ast = Parsetree_iter.iter_structure iter ast
  end)
