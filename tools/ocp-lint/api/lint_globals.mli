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

(** [Config] is a module which allow to create options for the configuration
    file and command-line arguments. *)
module Config : Lint_config.CONFIG

(** [plugins] is a global data structure where all plugins are registered.
    The keys of the structure are a [Lint_plugin_types.PLUGIN] and the value are
    a [LintMap.t]. *)
(* val plugins : *)
(*   ((module Lint_plugin_types.PLUGIN), Lint_types.lint) Hashtbl.t *)
val plugins :
  ((module Lint_plugin_types.PLUGIN), (module Lint_types.LINT) Lint_map.t)
    Hashtbl.t

val config_file : string
val olint_dirname : string

val default_patches : string list

(** [init no_db path] initialize the db and config modules with the path
     given in the command line *)
val init : bool -> string -> unit
