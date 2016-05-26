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

(** [load_plugins files] load dynamically files or files in a specific
    directory given in the command line with '--load' option. *)
val load_plugins : string list -> unit

(** [load_patches files] load dynamically patch files in a specific
    directory given in the command line with '--patches' option. *)
val load_patches : string list -> unit

(** [scan patches ?output_text no_db path] scan the [path] and start the
    registered plugins/linters on the files in this path. *)
val scan :
  ?output_text:string ->
  bool ->
  string ->
  unit

(** [init_db ()] create dir to dump the db. *)
val init_olint_dir : unit -> unit

(** [init no_db path] initialize the db and config modules with the path
     given in the command line *)
val init : bool -> string -> unit
