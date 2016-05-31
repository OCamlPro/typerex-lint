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

module Plugin = Plugin_typedtree.Plugin

module Linter = Plugin.MakeLint(struct
    let name = "Fully-Qualified Identifiers"
    let short_name = "fully_qualified_identifiers"
    let details = "Checks that external identifiers are fully qualified."
    let enable = true
  end)

type warning =
  | NotQualifiedEnough of string * string
  | AliasPersistentValue of string * string

let w_not_enough = Linter.new_warning
    ~id:1
    ~short_name:"not-qualified-enough"
    ~msg:"external identifier \"$ident\" is not fully qualified\n    (should be \"$path\")"

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | NotQualifiedEnough (ident, path) ->
        w_not_enough, ["ident", ident; "path", path]
      | AliasPersistentValue (ident, path) ->
        w_not_enough, ["ident", ident; "path", path]
  end)

let ignored_modules = Linter.create_option
    "ignored_modules"
    "Ignore unqualified identifiers coming from these modules"
    ""
    (SimpleConfig.list_option SimpleConfig.string_option)
    [ "Pervasives"; "StringCompat" ]

let ignore_operators = Linter.create_option
    "ignore_operators"
    "Ignore symbolic operators"
    ""
    SimpleConfig.bool_option
    true

let ignore_depth = Linter.create_option
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
        begin
          match check_path path with
          | None -> None
          | Some id -> Some (id ^ "." ^ field)
        end

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

    let rec is_persistent path =
      match path with
      | Path.Pident id ->
        Ident.persistent id
      | Path.Pdot(path, _,_) -> is_persistent path
      | Path.Papply _ -> false

    let enter_structure_item str =
      match str.str_desc with
      | Tstr_value (_, vbs) ->
        List.iter (fun vb ->
            match vb.vb_pat.pat_desc, vb.vb_expr.exp_desc with
            | Tpat_var (var,_), Texp_ident (Path.Pdot _ as path, _, _) ->
              if is_persistent path then
                Warnings.report vb.vb_loc
                  (AliasPersistentValue (Ident.name var, Path.name path))
            | _ -> ()
          ) vbs
      | _ -> ()

    end)
  in
  Iter.iter_structure ast

(* Registering a main entry to the linter *)
module Main = Linter.MakeInputCMT(struct

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
