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

open StringCompat
open SimpleConfig (* for !! *)

module Plugin = LintFabPlugin.Plugin

module Linter = Plugin.MakeLint(struct
    let name = "Good Practices"
    let short_name = "fabrice_good_practices"
    let details = "Checks some good practices."
  end)

type warnings =
  | UseNEWInsteadOfOLD of string * string
  | GoodPractice of string

let w_use_instead = Linter.new_warning
    [ Warning.kind_code ]
    ~short_name:"use-instead"
    ~msg:"Good practice: use \"$new\" instead of \"$old\""

let w_good_practice = Linter.new_warning
    [ Warning.kind_code ]
    ~short_name:"good-practice"
    ~msg:"Good practice: $advice"

module Warnings = struct
  let w_use_instead = Linter.instanciate w_use_instead
  let w_good_practice = Linter.instanciate w_good_practice

  let report loc = function
    | UseNEWInsteadOfOLD (new_expr, old_expr) ->
      w_use_instead loc ["new", new_expr; "old", old_expr]
    | GoodPractice advice ->
      w_good_practice loc ["advice", advice]

  end

let iter_structure ast =
  let module Iter = ParsetreeIter.MakeIterator(struct
      open Asttypes
      open Parsetree
      open Longident

    include ParsetreeIter.DefaultIteratorArgument

    let enter_pattern pat =
      match pat.ppat_desc with
      | Ppat_or(_, { ppat_desc = Ppat_any }) ->
        Warnings.report pat.ppat_loc
          (GoodPractice "avoid or-patterns with any-clause")
      | _ -> ()

    let enter_expression exp =
      match exp.pexp_desc with
      | Pexp_apply (
          { pexp_desc = Pexp_ident { txt = Lident "failwith" } },
          [
            "" (* NoLabel *),
            { pexp_desc = Pexp_apply (
                  { pexp_desc =
                      Pexp_ident
                        { txt = Ldot(Lident "Printf", "sprintf") }}, _ ) }
          ])
        ->
        Warnings.report exp.pexp_loc
          (UseNEWInsteadOfOLD ("Printf.kprintf failwith ARGS",
                               "failwith (Printf.sprintf ARGS)"))
      | _ -> ()
    end)
  in
  Iter.iter_structure ast

(* Registering a main entry to the linter *)
module Main = Linter.MakeInputStructure(struct

    let main = iter_structure
  end)
