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

(** A printing function over the type [Warning_types.warning].  *)
val print :
  Format.formatter ->
  string ->
  (Lint_utils.file_struct * ((string list) * string)) list ->
  int ->
  string ->
  Lint_db_types.t ->
  unit

(* [print_error fmt path db_errors] print error.*)
val print_error :
  Format.formatter ->
  string ->
  Lint_db_types.errors ->
  unit

(* [summary master_config file_config severity path pdetails
   no_db db db_errors]
   master_config: master config
   file_config: list of config file in the arborescence
   severity: only print warning higher that this
   path: path given by the user to filters results
   pdetails: verbose option
   no_db: flag if we are using a temp db
   db: results of the run
   db_errors: errors raise during the run
   print summary of the db. *)
val summary :
  master_config:string ->
  file_config:(Lint_utils.file_struct * ((string list) * string)) list ->
  severity:int ->
  path:string ->
  pdetails:bool ->
  no_db:bool ->
  db:Lint_db_types.t ->
  db_errors:Lint_db_types.errors ->
  unit

(* [print_only_new fmt path db] only print warning with Analyze tag.
   This will print only the warnings yield by the current run. *)
val print_only_new :
  Format.formatter ->
  string ->
  Lint_db_types.t ->
  unit

(* [verbose_info fmt db] print infos after linting a file. *)
val verbose_info : Format.formatter -> Lint_db_types.t -> unit

(* [debug_db db] print the full content of the db regardeless of options. *)
val debug_db : Lint_db_types.t -> unit
