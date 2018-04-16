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
    let version = "1"
    let short_name = "raw_syntax"
    let details = "Checks properties on raw syntax."
    let enabled = true
  end)

type warning =
  | AvoidParenAroundIfArgument
  | DirectMatchInCase of string
  | InconsistentListNotations
  | AvoidParenAroundConstrUniqArg of string
  | AvoidParenAroundSimpleExpression
  | AvoidParenAroundFunctionBody
  | ParenBlockAfterThen
  | ParenBlockAfterElse

let w_avoid_paren_around_if_argument = Linter.new_warning
    ~id:1
    ~short_name:"avoid_paren.if_argument"
    ~msg:"Avoid parentheses around if argument"
    ~severity:1
let w_direct_match_in_case = Linter.new_warning
    ~id:2
    ~short_name:"direct-match-in-case"
    ~msg:"$expr should be enclosed in parentheses"
    ~severity:7
let w_inconsistent_list_notations = Linter.new_warning
    ~id:3
    ~short_name:"inconsistent-list-notations"
    ~msg:"Inconsistent list notations"
    ~severity:1
let w_avoid_paren_around_constr_uniq_arg = Linter.new_warning
    ~id:4
    ~short_name:"avoid_paren.constr_uniq_arg"
    ~msg:"Avoid parentheses around constructor \"$constr\" uniq argument"
    ~severity:1
let w_avoid_paren_around_simple_expr = Linter.new_warning
    ~id:5
    ~short_name:"avoid_paren.simple_expr"
    ~msg:"Avoid parentheses around simple expression"
    ~severity:1
let w_avoid_paren_around_function_body = Linter.new_warning
    ~id:6
    ~short_name:"avoid_paren.function_body"
    ~msg:"Avoid parentheses around function body"
    ~severity:1
let w_paren_block_after_then = Linter.new_warning
    ~id:7
    ~short_name:"paren_block.then"
    ~msg:"Good practice: use '..then begin..end' instead of '..then (..)'"
    ~severity:1
let w_paren_block_after_else = Linter.new_warning
    ~id:8
    ~short_name:"paren_block.else"
    ~msg:"Good practice: use '..else begin..end' instead of '..else (..)'"
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | AvoidParenAroundIfArgument -> w_avoid_paren_around_if_argument, []
      | DirectMatchInCase expr -> w_direct_match_in_case, [ "expr", expr ]
      | InconsistentListNotations -> w_inconsistent_list_notations, []
      | AvoidParenAroundConstrUniqArg constr ->
        w_avoid_paren_around_constr_uniq_arg, [ "constr", constr ]
      | AvoidParenAroundSimpleExpression -> w_avoid_paren_around_simple_expr, []
      | AvoidParenAroundFunctionBody ->
        w_avoid_paren_around_function_body, []
      | ParenBlockAfterThen -> w_paren_block_after_then, []
      | ParenBlockAfterElse -> w_paren_block_after_else, []
  end)

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
    (* negative constants need parentheses *)
    | Pexp_constant (Pconst_integer (n,_)) when Int64.of_string n < 0L -> false
    | Pexp_constant (Pconst_float (n,_)) when float_of_string n < 0. -> false

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
    | Pexp_constant _
      -> true
    | _ -> false


    let check_str_item str =
      match str.pstr_desc with

      | Pstr_type (_rec, decls) ->
        List.iter (function
            | { ptype_kind = Ptype_variant variants } ->
              List.iter (fun pcd ->
                match pcd.pcd_res with
                | Some _ -> () (* GADT ? *)
                | None ->
                  match pcd.pcd_args with
                  (* Detect "A of (int)" *)
                  | Pcstr_tuple [ { ptyp_desc = Ptyp_tuple [ typ ] }] ->
                    begin
                      match typ.ptyp_desc with
                      | Ptyp_arrow _ -> ()
                      | _ ->
                        Warnings.report pcd.pcd_loc
                          (AvoidParenAroundConstrUniqArg pcd.pcd_name.txt)
                    end
                  | _ -> ()
                ) variants
            | _ -> ()
          ) decls
      | _ -> ()



  let single_line loc =
    let line_begin = loc.Location.loc_start.Lexing.pos_lnum in
    let line_end = loc.Location.loc_end.Lexing.pos_lnum in
    line_begin = line_end

  let check_ifthenelse cond ifthen ifelse =
    begin
      match cond with
      | { pexp_desc = Pexp_paren _ } ->
        Warnings.report cond.pexp_loc AvoidParenAroundIfArgument
      | _ -> ()
    end;
    begin
      match ifthen with
      | { pexp_desc = Pexp_paren _ ; pexp_loc }
        when not (single_line pexp_loc) ->
        Warnings.report pexp_loc ParenBlockAfterThen
      |  _ -> ()
    end;
    begin
      match ifelse with
      | Some { pexp_desc = Pexp_paren _; pexp_loc }
        when not(single_line pexp_loc) ->
        Warnings.report pexp_loc ParenBlockAfterElse
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
          Warnings.report exp.pexp_loc AvoidParenAroundSimpleExpression
      | Pexp_ifthenelse (cond, ifthen, ifelse) ->
        check_ifthenelse cond ifthen ifelse

      | Pexp_fun (_lab, _default, _pat,
                  { pexp_desc = Pexp_paren body}) ->
        if not (is_tuple body) then
          Warnings.report exp.pexp_loc
            AvoidParenAroundFunctionBody

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
        structure_item =
          (fun iterator str_item ->
            check_str_item str_item;
            default_iterator.structure_item iterator str_item);
      }
    in
    default_iterator.structure this_iterator str

  let main = LintParsingPlugin.wrap_syntax_error main

end

module Main = Linter.MakeInputML(MakeArg)
