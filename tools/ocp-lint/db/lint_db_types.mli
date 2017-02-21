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

(** This is the errors that will be stored in the database.
    Db_error: an error related to the database
    Plugin_error: an error that is raised by a plugin
    Sempatch_error: an error that is raised by sempatch
    Ocplint_error: an error that is raised by ocplint
 **)
type error =
  | Db_error of Lint_db_error.error
  | Plugin_error of Lint_plugin_error.error
  | Sempatch_error of string
  | Ocplint_error of string

(** This is the type of the database.

    The db is a hashtable. The key is the filename and the obj is the
    result.

    The result is the digest of the file and the results of the plugin's
    run.
    Result of a plugin's run =
    file_name1 -> [ plugin_name1 -> [ linter_name1 -> linter_result1,
                                      linter_name2 -> linter_result2,
                                      ... ],
                    plugin_name2 -> [],
                    ... ],
    file_name2 -> []
 **)

(** This allows us to determined if the warnings are gathered from a DB file
    or from an actual run. **)
type source = Cache | Analyse

(** This is how we register the result of a linter run.
    res_version: version of the linter, so we can ignore the outdated results
    res_source: Cache | Analyze
    res_options: assoc list of options of the plugin / linter, so we can
                 rerun when the options are changed by the user
    res_warnings: the warning that was raised by the linter
**)
type linter_result = {
  res_version : string;
  res_source : source;
  res_options : (string list * string) list;
  res_warnings: Lint_warning_types.warning list;
}
module StringMap : Map.S with type key = string
type linter_map = linter_result StringMap.t
type plugin_map = linter_map StringMap.t
type file_map = Digest.t * plugin_map

type t = (string, file_map) Hashtbl.t

(** Error set **)
module ErrorSet : Set.S with type elt = error
type error_set = ErrorSet.t

type errors = (string, error_set) Hashtbl.t

(** This is what we store in database file.
    db_version: version of the database, used to discard outdated results
    db_date: when the results where register, after some time we remove
             old results
    db_file_name: database filename is hash of the filename and its content
    db_file_pres: run results
    db_file_error: run errors
**)
type db_file_entry = {
  db_version : int;
  db_date : float;
  db_file_name : string;
  db_file_pres : plugin_map;
  db_file_error : error_set;
}

(* The current IO atom is a file entry. *)
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
  val load_file : Lint_utils.file_struct -> unit
  (* mark all the entries as coming from the cache (source =  Cache) *)
  val cache : unit -> unit
  (* save all the files from the database *)
  val save : unit -> unit
  (* merge the cached entries for the list of files into the
     in-memory database *)
  val merge : Lint_utils.file_struct list -> unit
  (* clear the in-memory database *)
  val reset : unit -> unit
  val print_debug : t -> unit
  (* [remove_entry file] remove the entry for file [file] from the
     in-memory database *)
  val remove_entry : string -> unit
  (* [add_entry ...] adds an empty entry in the in-memory database
     for a file (no warnings are declared) *)
  val add_entry : file_struct:Lint_utils.file_struct-> pname:string ->
    lname:string -> version:string -> unit
  (* [add_error file error] associates an error with a file *)
  val add_error : string -> error -> unit
  (* [clean ndays] clean all files on disk corresponding to files analysed
     more than [ndays] ago. *)
  val clean : int -> unit
  (* [update pname lname warning] adds this warning to the file
     provided in warning.loc in the in-memory database *)
  val update :
    pname:string -> lname:string -> warning:Lint_warning_types.warning -> unit
  (* [already_run file pname lname version] checks whether an analysis was
     already run on a particular file. *)
  val already_run :
    file_struct:Lint_utils.file_struct ->
    pname:string ->
    lname:string ->
    version:string ->
    bool
  (* whether the database contains a warning... *)
  val has_warning : unit -> bool
end
