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

module SyntaxLinter = Plugin.MakeLint(struct
    let name = "Fully-Qualified Identifiers"
    let short_name = "fully_qualified_identifiers"
    let details = "Checks that external identifiers are fully qualified."
  end)

type warnings =
  | IfParen

let w_ifparen = SyntaxLinter.new_warning
    [ Warning.kind_code ]
    ~short_name:"if-paren"
    ~msg:"if argument inside parentheses"

 module Warnings = struct
    let w_ifparen = SyntaxLinter.instanciate w_ifparen

    let report loc = function
      | IfParen -> w_ifparen loc []
  end

module Parsetree = LintFabOCaml_Parsetree
module Parse = LintFabOCaml_Parse
module Ast_iterator = LintFabOCaml_Ast_iterator

module Main = SyntaxLinter.MakeInputML(struct

    open Parsetree
    let main source =
      let ic = open_in source in
      let lexbuf = Lexing.from_channel ic in
      let str = LintFabOCaml_Parse.implementation lexbuf in
      let open Ast_iterator in
      let this_iterator =
        { default_iterator with
          expr =
            fun iterator expr ->
              begin
                match expr.pexp_desc with
                | Pexp_ifthenelse ({ pexp_desc = Pexp_paren _ }, _, _) ->

                  Warnings.report expr.pexp_loc IfParen
                | _ -> ()
              end;
              default_iterator.expr iterator expr
        }
      in
      default_iterator.structure this_iterator str

  end)
