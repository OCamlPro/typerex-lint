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
  (string * ((string list) * string)) list ->
  int ->
  string ->
  Lint_db_types.t ->
  unit

val print_error :
  Format.formatter ->
  string ->
  Lint_db_types.errors ->
  unit

val summary :
  string ->
  (string * ((string list) * string)) list ->
  int ->
  string ->
  bool ->
  Lint_db_types.t ->
  Lint_db_types.errors ->
  unit

val print_only_new :
  Format.formatter ->
  string ->
  Lint_db_types.t ->
  unit

val verbose_info : Format.formatter -> Lint_db_types.t -> unit

val debug_db : Lint_db_types.t -> unit
