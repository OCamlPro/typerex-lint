(**************************************************************************)
(*                                                                        *)
(*                        OCamlProT yperex                                *)
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
    let name = "Polymorphic function"
    let version = 1
    let short_name = "polymorphic_function"
    let details = "Detects all polymorphic functions."
    let enable = true
  end)

type warning = Poly of (string * string)

let w_poly = Linter.new_warning
    ~id:1
    ~short_name:"poly"
    ~msg:"Polymorphic function: '$fun' is used with '$types'."
    ~severity:10

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | Poly (f, v) ->
        w_poly, ["types", v; "fun", f]
  end)

let usual_suspects = [
  (* Pervasives *)
  "Pervasives.<";
  "Pervasives.>";
  "Pervasives.compare";
  "Pervasives.=";

  (* Hashtbl *)
  (* We only use 'Hashtbl.add', as we only want to print the creation of an
     Hashtbl.t *)
  "Hashtbl.add";

  (* List *)
  "List.mem";
  "List.mem_assoc";
  "List.remove_assoc";
  "List.assoc";
]

let stdlib =
  [
    "Arg";
    "Array";
    "Buffer";
    "BytesLabels";
    "Bytes";
    "Callback";
    "Char";
    "Complex";
    "Digest";
    "Filename";
    "Format";
    "Gc";
    "Genlex";
    "Hashtbl";
    "Int32";
    "Int64";
    "Lazy";
    "Lexing";
    "ListLabels";
    "List";
    "Map";
    "Marshal";
    "MoreLabels";
    "Nativeint";
    "Obj";
    "Oo";
    "Parsing";
    "Pervasives";
    "Printexc";
    "Printf";
    "Queue";
    "Random";
    "Scanf";
    "Set";
    "Sort";
    "Stack";
    "Stream";
    "String";
    "Sys";
    "Weak"
  ]

let simple_paths = [
  Predef.path_int;
  Predef.path_char;
  Predef.path_string;
  Predef.path_float;
  Predef.path_bool;
  Predef.path_unit;
  Predef.path_array;
  Predef.path_list;
  Predef.path_option;
  Predef.path_nativeint;
  Predef.path_int32;
  Predef.path_int64;
]

let rec simple_function ty =
  let open Types in
  match ty.desc with
  | Tarrow (_, ty1, ty2, _) -> simple_type ty1 && simple_function ty2
  | Tlink ty -> simple_function ty
  | _ -> simple_type ty

and simple_type ty =
  let open Types in
  match ty.desc with
  | Tvar _ -> false
  | Tarrow (_, ty1, ty2, _) -> false
  | Ttuple list -> List.for_all simple_type list
  | Tconstr (path, list, _) ->
    simple_path path && List.for_all simple_type list
  | Tlink ty -> simple_type ty
  | _ -> false

and simple_path path = List.mem path simple_paths

let simple_function name ty = simple_function ty

let new_occurrence f ty loc =
  Printtyp.reset ();
  Buffer.clear Format.stdbuf;
  Printtyp.type_expr Format.str_formatter ty;
  let name = Format.flush_str_formatter () in
  if not (simple_function name ty) then
    Warnings.report loc (Poly (f, name))

let iter =
 let module ForIterator = struct
   open Typedtree
   open Location

   include Typedtree_iter.DefaultIteratorArgument

   let enter_expression exp =
     match exp.exp_desc with
     | Texp_apply ({ exp_desc = Texp_ident (path, lid, vdesc);
                     exp_loc = loc } as f, args) ->
       let name = Path.name path in
       let modname =
         match Longident.flatten lid.txt with
         | modname :: _ -> modname
         | _ -> "" in
       if List.mem name usual_suspects || not (List.mem modname stdlib) then
         new_occurrence name f.exp_type loc
     | _ -> ()
 end in
 (module ForIterator : Typedtree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module Main = Linter.MakeInputCMT(struct
    let main cmt = Typedtree_iter.iter_structure iter cmt
  end)
