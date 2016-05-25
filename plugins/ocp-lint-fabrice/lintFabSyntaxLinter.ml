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

(*
Detects:
* if ( EXPR ) then ... else ...
* fun x -> ( BODY )
*

Added:
| Pexp_paren
| Pexp_begin
Lident "begin end" (unit "()" created with "begin end")
*)


open StringCompat
open SimpleConfig (* for !! *)

module Plugin = LintFabPlugin.Plugin

module SyntaxLinter = Plugin.MakeLint(struct
    let name = "Raw Syntax"
    let short_name = "raw_syntax"
    let details = "Checks properties on raw syntax."
  end)

type warnings =
  | AvoidParen of string
  | DirectMatchInCase of string
  | InconsistentListNotations

let w_avoid_paren = SyntaxLinter.new_warning
    [ Warning.kind_code ]
    ~short_name:"avoid-paren"
    ~msg:"Avoid parentheses around $expr"
let w_direct_match_in_case = SyntaxLinter.new_warning
    [ Warning.kind_code ]
    ~short_name:"direct-match-in-case"
    ~msg:"$expr should be enclosed in parenthese"
let w_inconsistent_list_notations = SyntaxLinter.new_warning
    [ Warning.kind_code ]
    ~short_name:"inconsistent-list-notations"
    ~msg:"Inconsistent list notations"

 module Warnings = struct
    let w_avoid_paren = SyntaxLinter.instanciate w_avoid_paren
    let w_direct_match_in_case = SyntaxLinter.instanciate w_direct_match_in_case
    let w_inconsistent_list_notations =
      SyntaxLinter.instanciate w_inconsistent_list_notations

    let report loc = function
      | AvoidParen where -> w_avoid_paren loc [ "expr", where  ]
      | DirectMatchInCase expr -> w_direct_match_in_case loc [ "expr", expr ]
      | InconsistentListNotations -> w_inconsistent_list_notations loc []
  end

module Asttypes = LintFabOCaml_Asttypes
module Parsetree = LintFabOCaml_Parsetree
module Parse = LintFabOCaml_Parse
module Ast_iterator = LintFabOCaml_Ast_iterator


module MakeArg = struct

  open Asttypes
  open Parsetree
  open Longident

  let check_expr exp =
    begin
      match exp.pexp_desc with
      | Pexp_ifthenelse ({ pexp_desc = Pexp_paren _ }, _, _) ->

        Warnings.report exp.pexp_loc (AvoidParen "if argument")
      | Pexp_fun (_lab, _default, _pat,
                  { pexp_desc = Pexp_paren _}) ->
        Warnings.report exp.pexp_loc
          (AvoidParen "function body")


      (* x :: [y] instead of [x;y] *)
      | Pexp_construct({ txt = Lident "::" },
                       Some ({ pexp_desc =
                                 Pexp_tuple [_;
                                             { pexp_desc = Pexp_list (_::_) }
                                            ]})) ->
        Warnings.report exp.pexp_loc
          InconsistentListNotations

      | _ -> ()
    end

  let check_case pc =
    match pc.pc_rhs.pexp_desc with
    | Pexp_match _ ->
      Warnings.report pc.pc_rhs.pexp_loc (DirectMatchInCase "match")
    | Pexp_try _ ->
      Warnings.report pc.pc_rhs.pexp_loc (DirectMatchInCase "try")
    | Pexp_fun _ ->
      Warnings.report pc.pc_rhs.pexp_loc (DirectMatchInCase "fun")
    | _ -> ()

  let main source =
    Printf.eprintf "Parsing %s\n%!" source;
    let ic = open_in source in
    let lexbuf = Lexing.from_channel ic in
    let str =
      try
        LintFabOCaml_Parse.implementation lexbuf
      with exn ->
        close_in ic;
        raise exn
    in
    let open Ast_iterator in
    let this_iterator =
      { default_iterator with
        expr =
          (fun iterator exp ->
             check_expr exp;
             default_iterator.expr iterator exp);
        case =
          (fun iterator case ->
             check_case case;
             default_iterator.case iterator case);
      }
    in
    default_iterator.structure this_iterator str

end

module Main = SyntaxLinter.MakeInputML(MakeArg)
