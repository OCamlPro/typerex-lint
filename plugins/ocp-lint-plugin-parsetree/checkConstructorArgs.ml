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

module Plugin = Plugin_parsetree.Plugin

module Linter = Plugin.MakeLint(struct
    let name = "Check Constructor Arguments"
    let version = "1"
    let short_name = "check_constr_args"
    let details = "Check that constructor arguments are ok."
    let enable = true
  end)

type warning =
  | TupleArgWithParen of string
  (*  | SingleArgWithParen of string *)

(* Performance issue *)
let w_tuple_arg_with_paren = Linter.new_warning
    ~id:1
    ~short_name:"tuple-arg-with-paren"
    ~msg:"Constructor \"$constr\" has a tuple argument instead of multiple arguments"
    ~severity:9

(* This one is not detected: probably discarded by the parser
let w_single_arg_with_paren = Linter.new_warning
    ~id:2
    ~short_name:"single-arg-with-paren"
    ~msg:"Constructor \"$constr\" with a single argument should not be between parens"
*)

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | TupleArgWithParen name -> w_tuple_arg_with_paren, ["constr", name]
      (*  | SingleArgWithParen name -> w_single_arg_with_paren, ["constr", name] *)
  end)

let iter_structure ast =
  let module Iter = Parsetree_iter.MakeIterator(struct
      open Asttypes
      open Parsetree
      open Longident

    include Parsetree_iter.DefaultIteratorArgument

    let enter_structure_item str =
      match str.pstr_desc with

#if OCAML_VERSION < "4.03.0"
      | Pstr_type decls ->
#else
      | Pstr_type (_, decls) ->
#endif
        List.iter (function
            | { ptype_kind = Ptype_variant variants } ->
              List.iter (fun pcd ->
                match pcd.pcd_res with
                | Some _ -> () (* GADT ? *)
                | None ->
                  match pcd.pcd_args with
(*
                  | [ { ptyp_desc = Ptyp_tuple [_] }] ->
                    Warnings.report pcd.pcd_loc
                      (SingleArgWithParen pcd.pcd_name.txt)
*)
#if OCAML_VERSION < "4.03.0"
                  | [ { ptyp_desc = Ptyp_tuple _ }] ->
#else
                  | Pcstr_tuple _ ->
#endif
                    Warnings.report pcd.pcd_loc
                      (TupleArgWithParen pcd.pcd_name.txt)
                  | _ -> ()
                ) variants
            | _ -> ()
          ) decls
      | _ -> ()


    end)
  in
  Iter.iter_structure ast

(* Registering a main entry to the linter *)
module Main = Linter.MakeInputStructure(struct

    let main = iter_structure
  end)

(* Example: *)
type t =
  | A of (int)
  | B of int
  | C of (int * int)
  | D of int * int
