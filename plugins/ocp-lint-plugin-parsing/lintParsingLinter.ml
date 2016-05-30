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

Added:
| Pexp_paren
| Pexp_begin
Lident "begin end" (unit "()" created with "begin end")
*)


open StringCompat
open SimpleConfig (* for !! *)

module Plugin = LintParsingPlugin.Plugin

module Linter = Plugin.MakeLint(struct
    let name = "Raw Syntax"
    let short_name = "raw_syntax"
    let details = "Checks properties on raw syntax."
    let enable = true
  end)

type warnings =
  | AvoidParen of string
  | DirectMatchInCase of string
  | InconsistentListNotations
  | UseNEWInsteadOfOLD of string * string

let w_avoid_paren = Linter.new_warning
    [ Lint_warning.kind_code ]
    ~short_name:"avoid-paren"
    ~msg:"Avoid parentheses around $expr"
let w_direct_match_in_case = Linter.new_warning
    [ Lint_warning.kind_code ]
    ~short_name:"direct-match-in-case"
    ~msg:"$expr should be enclosed in parentheses"
let w_inconsistent_list_notations = Linter.new_warning
    [ Lint_warning.kind_code ]
    ~short_name:"inconsistent-list-notations"
    ~msg:"Inconsistent list notations"
let w_use_instead = Linter.new_warning
    [ Lint_warning.kind_code ]
    ~short_name:"use-instead"
    ~msg:"Good practice: use \"$new\" instead of \"$old\""


 module Warnings = struct
    let w_avoid_paren = Linter.instanciate w_avoid_paren
    let w_direct_match_in_case = Linter.instanciate w_direct_match_in_case
    let w_inconsistent_list_notations =
      Linter.instanciate w_inconsistent_list_notations
    let w_use_instead =
      Linter.instanciate w_use_instead

    let report loc = function
      | AvoidParen where -> w_avoid_paren loc [ "expr", where  ]
      | DirectMatchInCase expr -> w_direct_match_in_case loc [ "expr", expr ]
      | InconsistentListNotations -> w_inconsistent_list_notations loc []
      | UseNEWInsteadOfOLD (new_expr, old_expr) ->
        w_use_instead loc ["new", new_expr; "old", old_expr]

  end

module Asttypes = LintParsing_Asttypes
module Parsetree = LintParsing_Parsetree
module Parse = LintParsing_Parse
module Ast_iterator = LintParsing_Ast_iterator


module MakeArg = struct

  open Asttypes
  open Parsetree
  open Longident

  let should_not_paren exp =
    match exp.pexp_desc with
    | Pexp_constant _
    | Pexp_ident _
    | Pexp_begin _
    | Pexp_paren _
    | Pexp_list _
    (*    | Pexp_tuple _ : tuples with parens are OK *)
    | Pexp_construct (_, None)
    | Pexp_variant (_, None)
    | Pexp_record _
    | Pexp_field _
    | Pexp_array _
    | Pexp_while _
    | Pexp_for _
    | Pexp_constraint _
    | Pexp_coerce _
    | Pexp_override _
    | Pexp_pack _
    | Pexp_extension _
      -> true
    | _ -> false

  let single_line loc =
    let line_begin = loc.Location.loc_start.Lexing.pos_lnum in
    let line_end = loc.Location.loc_end.Lexing.pos_lnum in
    line_begin = line_end

  let check_ifthenelse cond ifthen ifelse =
    begin
      match cond with
      | { pexp_desc = Pexp_paren _ } ->
        Warnings.report cond.pexp_loc (AvoidParen "if argument")
      | _ -> ()
    end;
    begin
      match ifthen with
      | { pexp_desc = Pexp_paren _ ; pexp_loc }
        when not (single_line pexp_loc) ->
        Warnings.report pexp_loc
          (UseNEWInsteadOfOLD ("then begin ... end", "then ( .. )"))
      |  _ -> ()
    end;
    begin
      match ifelse with
      | Some { pexp_desc = Pexp_paren _; pexp_loc }
        when not(single_line pexp_loc) ->
        Warnings.report pexp_loc
          (UseNEWInsteadOfOLD ("else begin ... end", "else ( .. )"))
      |  _ -> ()
    end

  let is_tuple body =
    match body.pexp_desc with
    | Pexp_tuple _ -> true
    | _ -> false

  let check_expr exp =
    begin
      match exp.pexp_desc with
      | Pexp_paren exp ->
        if should_not_paren exp then
          Warnings.report exp.pexp_loc (AvoidParen "simple expression")
      | Pexp_ifthenelse (cond, ifthen, ifelse) ->
        check_ifthenelse cond ifthen ifelse

      | Pexp_fun (_lab, _default, _pat,
                  { pexp_desc = Pexp_paren body}) ->
        if not (is_tuple body) then
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
    Printf.eprintf "%s.%s: parsing %s\n%!"
      Plugin.short_name Linter.short_name source;
    let ic = open_in source in
    Location.input_name := source;
    let lexbuf = Lexing.from_channel ic in
    Location.init lexbuf source;
    let str =
      try
        LintParsing_Parse.implementation lexbuf
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

module Main = Linter.MakeInputML(MakeArg)
