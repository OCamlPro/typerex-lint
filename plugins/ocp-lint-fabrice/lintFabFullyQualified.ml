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

module FullyQualifiedChecker = Plugin.MakeLint(struct
    let name = "Fully-Qualified Identifiers"
    let short_name = "fully_qualified_identifiers"
    let details = "Checks that external identifiers are fully qualified."
  end)

type warnings =
  | NotQualifiedEnough of string * string

let w_not_enough = FullyQualifiedChecker.new_warning
    [ Warning.kind_code ]
    ~short_name:"not-qualified-enough"
    ~msg:"external identifier \"$ident\" is not fully qualified\n    (should be \"$path\")"

 module Warnings = struct
    let w_not_enough = FullyQualifiedChecker.instanciate w_not_enough

    let report loc = function
      | NotQualifiedEnough (ident, path) ->
        w_not_enough loc ["ident", ident; "path", path]
  end

let ignored_modules = FullyQualifiedChecker.create_option
    "ignored_modules"
    "Ignore unqualified identifiers coming from these modules"
    ""
    (SimpleConfig.list_option SimpleConfig.string_option)
    [ "Pervasives"; "StringCompat" ]

let ignore_operators = FullyQualifiedChecker.create_option
    "ignore_operators"
    "Ignore symbolic operators"
    ""
    SimpleConfig.bool_option
    true

let ignore_depth = FullyQualifiedChecker.create_option
    "ignore_depth"
    "Ignore qualified identifiers of that depth, not fully qualified"
    ""
    SimpleConfig.int_option
    2

let iter_structure ast =
  let ignored_modules =
    StringSet.of_list !!ignored_modules
  in
  let module Iter = TypedtreeIter.MakeIterator(struct
    include TypedtreeIter.DefaultIteratorArgument

    open Typedtree
    open Asttypes

    let rec check_path path =
      match path with
      | Path.Pident id ->
        let id = Ident.name id in
        if StringSet.mem id ignored_modules then
          None
        else Some id
      | Path.Papply _ -> None
      | Path.Pdot(path, field, _) ->
        match check_path path with
        | None -> None
        | Some id -> Some (id ^ "." ^ field)

    let is_operator s =
      match s.[0] with
        'a'..'z' | 'A'..'Z' -> false
      | _ -> true

    let enter_expression exp =
      match exp.exp_desc with
      | Texp_ident (path,id_loc,val_desc) ->
        begin
          match check_path path with
          | None -> ()
          | Some path ->
            let id_loc = Longident.flatten id_loc.txt in
            if List.length id_loc < !!ignore_depth then
              let id_loc = String.concat "." id_loc in
              if not (!!ignore_operators && is_operator id_loc)
              && path <> id_loc then begin
                Warnings.report exp.exp_loc
                  (NotQualifiedEnough (id_loc, path));
            end
        end
      | _ -> ()
    end)
  in
  Iter.iter_structure ast

(* Registering a main entry to the linter *)
module Main = FullyQualifiedChecker.MakeInputCMT(struct

    open Cmt_format

    let main cmt =
      match cmt.cmt_annots with
      | Implementation str -> iter_structure str
      | Packed _ -> ()
      | Interface sg -> ()
      | Partial_implementation _ ->
        failwith "Bad .cmt file"
      | Partial_interface _ ->
        failwith "Bad .cmti file"
  end)
