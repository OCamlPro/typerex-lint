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

let details =
  "Checks that we do not use a list function on a singleton element."

module ListOnSingleton = Plugin_parsetree.Plugin.MakeLint(struct
    let name = "List function on singleton"
    let short_name = "code_list_on_singleton"
    let details = details
    let enable = false
  end)

type warning =
  | Empty of string
  | Singleton of string

let w_empty = ListOnSingleton.new_warning
    ~id:1
    ~short_name:"list_function_on_empty"
    ~msg:"$fun is used on an empty list."

let w_singleton = ListOnSingleton.new_warning
    ~id:2
    ~short_name:"list_function_on_singleton"
    ~msg:"$fun is used on a singleton."

module Warnings = ListOnSingleton.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | Empty funct -> w_empty, [("fun", funct)]
      | Singleton funct -> w_singleton, [("fun", funct)]
  end)

let is_singleton f args =
  let open Parsetree in
  let _, args = args in
  match args.pexp_desc with
  | Pexp_construct (loc, eopt) ->
    begin
      match eopt with
      | None -> Warnings.report loc.Asttypes.loc (Empty f)
      | Some expr ->
        begin match expr.pexp_desc with
          | Pexp_tuple list ->
            List.iter (fun elt ->
                match elt.pexp_desc with
                | Pexp_construct (loc, eopt) ->
                  begin match eopt with
                    | None -> Warnings.report elt.pexp_loc (Singleton f)
                    | Some _ -> ()
                  end
                | _ -> ())
              list
          | _ -> ()
        end
    end
  | _ -> ()

let check_args f i args =
  try
    is_singleton f (List.nth args i)
  with _ ->
    () (* Partial application or |> ? *)

let check_fun f args =
  match f with
  | "List.rev"
  | "List.hd"
  | "List.tl"
  | "List.flatten"
  | "List.split"
  | "List.concat"
  | "|>" -> check_args f 0 args
  | "List.map"
  | "List.mapi"
  | "String.concat"
  | "List.fold_right"
  | "List.iter"
  | "List.iteri"
  | "List.for_all"
  | "List.exists"
  | "List.mem"
  | "List.memq"
  | "List.find"
  | "List.filter"
  | "List.find_all"
  | "List.assoc"
  | "List.assq"
  | "List.mem_assoc"
  | "List.mem_assq"
  | "List.remove_assoc"
  | "List.remove_assq"
  | "List.sort"
  | "List.stable_sort"
  | "List.fast_sort"
  | "List.sort_uniq" -> check_args f 1 args
  | "List.fold_left" -> check_args f 2 args
  | _ -> ()

let iter =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument

    let enter_expression expr =
      let open Parsetree in
      let open Asttypes in
      match expr.pexp_desc with
      | Pexp_apply (f, args) ->
        begin
          match f.pexp_desc with
          | Pexp_ident id ->
            let flat = Longident.flatten id.txt in
            let str_fun = String.concat "." flat in
            check_fun str_fun args
          | _ -> ()
        end
      | _ -> ()

  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Registering a main entry to the linter *)
module MainML = ListOnSingleton.MakeInputStructure(struct
    let main ast = Parsetree_iter.iter_structure iter ast
  end)
