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

type file_struct = {
  name : string;
  norm : string;
  hash : string;
  cmt : string option;
  ignored : string list;
}

(** [iter_files ~recdir apply dirname] iters on the given dirname and
    apply the function [apply] to all the found files (relative names
    to the [dirname] directory). If [recdir] is set to false it only
    scans the files in the given directory and does not iter
    recursively in the subdirectories. *)
val iter_files : ?recdir:bool -> (string -> unit) -> string -> unit

(** [substitute str substs] subsitutes the string [str] with the given
     substitutes list [substs]. It replaces all the '$ID' by the
     matching string in the list. The function is not efficient on
    long lists of arguments. *)
val substitute : string -> (string * string) list -> string

(** [group_by fct lst] groups the elements of the list [lst] by their
    result of the function [fct].
 *)
val group_by: ('a -> 'b) -> 'a list -> ('b * 'a list) list

(** [concat sep arr] Same as String.concat 
    but on string array. *)
val array_concat : string -> string array -> string

(** [absolute_path root filename] give the absolute path of a file. *)
val absolute_path : string -> string -> string

(** [relative root file] give the relative path to [root] of [file].
    [root] and [file] are expected to be absolute filenames.
*)
val relative_path : string -> string -> string

(** [find_root root_dir basenames] recurcively looks for the basenames in the
     path which marks the root of a project and contains the db file. *)
val find_root : FileGen.t -> string list -> FileGen.t

(** [is_in_path root file path] checks if the file is in the path. *)
val is_in_path : string -> string -> string -> bool

(** [read_file file] get the content of the file as a string. *)
val read_file : string -> string

(** [lines_count_of_file file] get the number of lines of the file. *)
val lines_count_of_file : string -> int

(** [normalize_path root file] normalize a path given the root dir. *)
val normalize_path : root:string -> file:string -> string

(** [mk_temp_dir prefix] make a temporary directory to store the db. *)
val mk_temp_dir : string -> string

val db_hash : string -> string

(** [split_sources sources] will match the .cmt files to their .ml files.
    This return 2 lists : - the association list (file.ml, file.cmt)
                          - the sources without the .cmt *)
val split_sources :
  string list -> ((string * string * string) list * string list)

val mk_file_struct :
  string ->
  string ->
  string list ->
  (string * string * string) list ->
  file_struct

val save_file_struct : string -> file_struct -> string

val read_file_struct : string -> file_struct
