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

(** Values representing the kind of warnings. See [Lint_warning_types.kind] for more
    details. *)
val kind_code : Lint_warning_types.kind
val kind_typo : Lint_warning_types.kind
val kind_interface : Lint_warning_types.kind
val kind_metrics : Lint_warning_types.kind
val new_kind : string -> Lint_warning_types.kind

(** [kind_to_string kind] returns the string representation of
    [Lint_warning_types.kind]. *)
val kind_to_string : Lint_warning_types.kind -> string

(**** Warnings data structure. ****)
module Warning : sig

  (** [add loc id kinds short_name message wset] adds the warning to [wset] with
      the location [loc], warning number [id], kinds [kinds], a short message
      [short_message] which will be display at command line or in configuration
      file and the message [message] which represents the message displayed when
      the warning will be emit.*)
  val add :
    string ->
    string ->
    Location.t ->
    int ->
    Lint_warning_types.warning_declaration ->
    string ->
    unit

  (** [add_warning warning wset] adds the warning [warning] to [wset]. *)
  val add_warning : string -> string -> Lint_warning_types.warning -> unit

end
