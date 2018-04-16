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

module Plugin = LintParsingPlugin.Plugin

module Linter = Plugin.MakeLint(struct
    let name = "Check Tuple"
    let version = "1"
    let short_name = "check_tuple"
    let details = "Check some properties on tuple"
    let enabled = true
  end)


type warning =
  | RecordRatherThanTuple

let w_record_rather_than_tuple = Linter.new_warning
    ~id:1
    ~short_name:"use_record_rather_than_tuple"
    ~msg:"Should use a record rather than a tuple."
    ~severity:1

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | RecordRatherThanTuple -> w_record_rather_than_tuple, []
  end)

module Asttypes = LintParsing_Asttypes
module Parsetree = LintParsing_Parsetree
module Parse = LintParsing_Parse
module Ast_iterator = LintParsing_Ast_iterator


module MakeArg = struct

  open Asttypes
  open Parsetree
  open Longident

  let process_tuple loc =
    Warnings.report loc (RecordRatherThanTuple)

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

        typ =
          (fun iterator typ ->
           begin match typ.ptyp_desc with
           | Ptyp_tuple _ ->
              process_tuple typ.ptyp_loc;
              default_iterator.typ iterator typ
           | _ ->
              default_iterator.typ iterator typ
           end);

        pat =
          (fun iterator pat ->
           let rec aux pat =
             begin match pat.ppat_desc with
             | Ppat_tuple _ ->
                process_tuple pat.ppat_loc;
                default_iterator.pat iterator pat
             | Ppat_construct (
                   {txt = Longident.Lident "::"},
                   Some ({ppat_desc = Ppat_tuple args; _})
                 ) ->
                List.iter aux args
             | _ ->
                default_iterator.pat iterator pat
             end
           in aux pat);

        expr =
          (fun iterator exp ->
           let rec aux exp =
             begin match exp.pexp_desc with
             | Pexp_tuple _ ->
                process_tuple exp.pexp_loc;
                default_iterator.expr iterator exp
             | Pexp_construct (
                   {txt = Longident.Lident "::"},
                   Some ({pexp_desc = Pexp_tuple args; _})
                 ) ->
                List.iter aux args
             | _ ->
                default_iterator.expr iterator exp
             end
           in aux exp);

      }
    in
    default_iterator.structure this_iterator str

  let main = LintParsingPlugin.wrap_syntax_error main

end

module Main = Linter.MakeInputML(MakeArg)
