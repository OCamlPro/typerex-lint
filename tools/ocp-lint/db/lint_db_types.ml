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

module StringMap = Map.Make(String)
module ErrorSet = Set.Make(struct
  let compare = Pervasives.compare
  type t = error
  end)

type source = Cache | Analyse

type warning_list =
  source * (string list * string) list * Lint_warning_types.warning list
type linter_map = warning_list StringMap.t
type plugin_map = linter_map StringMap.t
type file_map = Digest.t * plugin_map

type error_map = ErrorSet.t

type t = (string, file_map) Hashtbl.t
type errors = (string, error_map) Hashtbl.t




module type DATABASE_IO = sig
  val load : string -> (string * plugin_map)
  val save : string -> (string * plugin_map) -> unit
end

module type DATABASE = sig
  val db : t
  val init : string list -> File.t -> unit
  val load : string -> t
  val save : unit -> unit
  val reset : unit -> unit
  val remove_entry : string -> unit
  val add_entry : string -> string -> string -> unit
  val add_error : string -> error -> unit
  val clean : string list -> unit
  val update : string -> string -> Lint_warning_types.warning -> unit
  val already_run : string -> string -> string -> bool
  val has_warning : unit -> bool
end
