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

type error =
  | Db_error of Lint_db_error.error
  | Plugin_error of Lint_plugin_error.error
  | Sempatch_error of string
  | Ocplint_error of string

module StringMap = Map.Make(String)
module ErrorSet = Set.Make(struct
    let compare = Pervasives.compare
    type t = error
  end)

type source = Cache | Analyse

type linter_result = {
  res_version : int;
  res_source : source;
  res_options : (string list * string) list;
  res_warnings: Lint_warning_types.warning list;
}
type linter_map = linter_result StringMap.t
type plugin_map = linter_map StringMap.t
type file_map = Digest.t * plugin_map

type error_set = ErrorSet.t

type t = (string, file_map) Hashtbl.t
type errors = (string, error_set) Hashtbl.t

module type DATABASE_IO = sig
  val load : string -> int * float * string * plugin_map * error_set
  val save : string -> int * float * string * plugin_map * error_set -> unit
end

module type DATABASE = sig
  val db : t
  val db_errors : errors
  val root : string ref
  val init : File.t -> unit
  val load : string -> t
  (* Load the file _olint/XXX containing the db for that file *)
  val load_file : string -> unit
  val cache : unit -> unit
  val save : unit -> unit
  val merge : string list -> unit
  val reset : unit -> unit
  val print_debug : t -> unit
  val remove_entry : string -> unit
  val add_entry : string -> string -> string -> int -> unit
  val add_error : string -> error -> unit
  val clean : int -> unit
  val update : string -> string -> Lint_warning_types.warning -> unit
  val already_run : string -> string -> string -> int -> bool
  val has_warning : unit -> bool
end
