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

open SimpleConfig (* for !! *)

let default_max = 30
let default_min = 2

let details =
  Printf.sprintf
    "Checks that every identifier has a minimum and a maximum length. \
     Usually, short names implies that the code is harder to read and \
     understand. \n \
     The default value for short identifier is %d and for the long identifier \
     is %d.\n"
    default_min
    default_max

module CodeIdentifierLength = Plugin_parsetree.Plugin.MakeLint(struct
    let name = "Code Identifier Length"
    let short_name = "code_identifier_length"
    let details = details
    let enable = false
  end)

type warning =
  | Short of (int * string)
  | Long of (int * string)

let w_too_short = CodeIdentifierLength.new_warning
    ~id:1
    ~short_name:"identifier_too_short"
    ~msg:"$id is too short: it should be at least of size '$size'."

let w_too_long = CodeIdentifierLength.new_warning
    ~id:2
    ~short_name:"identifier_too_long"
    ~msg:"$id is too long: it should be at most of size '$size'."

module Warnings = CodeIdentifierLength.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | Short (min, id) ->
        w_too_short, [("id", id); ("size", string_of_int min)]
      | Long (max, id) -> w_too_long, [("id", id); ("size", string_of_int max)]
  end)

let iter min_identifier_length max_identifier_length =
  let module IterArg = struct
    include Parsetree_iter.DefaultIteratorArgument

    let enter_pattern pat =
      let open Parsetree in
      let open Asttypes in
      begin match pat.ppat_desc with
        | Ppat_var ident ->
          let id_str = ident.txt in
          let id_loc = ident.loc in
          let id_len = String.length id_str in
          if id_len < min_identifier_length then
            Warnings.report id_loc (Short (min_identifier_length, id_str));
          if id_len > max_identifier_length then
            Warnings.report id_loc (Long (max_identifier_length, id_str))
        | _ -> ()
      end
  end in
  (module IterArg : Parsetree_iter.IteratorArgument)

(* Defining/Using option from configuration file / command line *)
let min_identifier_length = CodeIdentifierLength.create_option
    "min_identifier_length"
    "Identifiers with a shorter name will trigger a warning"
    "Identifiers with a shorter name will trigger a warning"
    SimpleConfig.int_option
    default_min

let max_identifier_length = CodeIdentifierLength.create_option
     "max_identifier_length"
    "Identifiers with a longer name will trigger a warning"
    "Identifiers with a longer name will trigger a warning"
    SimpleConfig.int_option
    default_max

(* Registering a main entry to the linter *)
module MainML = CodeIdentifierLength.MakeInputStructure(struct
    let main ast =
      Parsetree_iter.iter_structure
        (iter !!min_identifier_length !!max_identifier_length) ast
  end)
