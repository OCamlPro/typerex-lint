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

type db_file_entry = {
  db_version : int;
  db_date : float;
  db_file_name : string;
  db_file_pres : plugin_map;
  db_file_error : error_set;
}

module type DATABASE_IO = sig

  val load : string -> db_file_entry
  val save : string -> db_file_entry -> unit
end

module type DATABASE = sig
  val db : t
  val db_errors : errors
  val root : string ref
  val init : File.t -> unit

  (* [load dir] : load the entire database composed of files from [dir]. *)
  val load : string -> t
  (* [load_file file] Load the file _olint/XXX containing the db for
     the file [file]. *)
  val load_file : string -> unit

  (* mark all the entries as coming from the cache (source =  Cache) *)
  val cache : unit -> unit

  (* save all the files from the database *)
  val save : unit -> unit

  (* merge the cached entries for the list of files into the
     in-memory database *)
  val merge : string list -> unit

  (* clear the in-memory database *)
  val reset : unit -> unit
  val print_debug : t -> unit

  (* [remove_entry file] remove the entry for file [file] from the
     in-memory database *)
  val remove_entry : string -> unit

  (* [add_entry ...] adds an empty entry in the in-memory database
     for a file (no warnings are declared) *)
  val add_entry :
    file:string ->
    pname:string ->
    lname:string ->
    version:int -> unit

  (* [add_error file error] associates an error with a file *)
  val add_error : string -> error -> unit

  (* [clean ndays] clean all files on disk corresponding to files analysed
     more than [ndays] ago. *)
  val clean : int -> unit

  (* [update pname lname warning] adds this warning to the file
     provided in warning.loc in the in-memory database *)
  val update : string -> string -> Lint_warning_types.warning -> unit

  (* [already_run pname lname file version] checks whether an analysis was
     already run on a particular file. *)
  val already_run : string -> string -> string -> int -> bool

  (* whether the database contains a warning... *)
  val has_warning : unit -> bool
end
