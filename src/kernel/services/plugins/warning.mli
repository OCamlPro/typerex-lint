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

(** Values representing the kind of warnings. See [Warning_types.kind] for more
    details. *)
val kind_code : Warning_types.kind
val kind_typo : Warning_types.kind
val kind_interface : Warning_types.kind
val kind_metrics : Warning_types.kind
val new_kind : string -> Warning_types.kind

(** [kind_to_string kind] returns the string representation of
    [Warning_types.kind]. *)
val kind_to_string : Warning_types.kind -> string

(**** Warnings data structure. ****)

module WarningDeclaration : sig
  (** Abstract type representation the warning declaration data structure. *)
  type t

  (** The empty set of warning declaration. *)
  val empty : unit -> t

  (** [add wdecl wdecl_set] adds the warning declaration [wdecl] to [wset]. *)
  val add : Warning_types.warning_declaration -> t -> unit
end

module Warning : sig
  (** Abstract type representation the warnings data structure. *)
  type t

  (** The empty set of warning. *)
  val empty : unit -> t

  (** [add loc id kinds short_name message wset] adds the warning to [wset] with
      the location [loc], warning number [id], kinds [kinds], a short message
      [short_message] which will be display at command line or in configuration
      file and the message [message] which represents the message displayed when
      the warning will be emit.*)
  val add :
    Location.t -> int -> Warning_types.warning_declaration -> string -> t -> unit

  (** [add_warning warning wset] adds the warning [warning] to [wset]. *)
  val add_warning : Warning_types.warning -> t -> unit

  (** [length wset] returns the size of [wset].  *)
  val length : t -> int

  (** [iter f wset] applies [f] in turn to all elements of [wset]. The elements
      of [wset] are presented to [f] in increasing order with respect to the
      ordering over the type of elements. *)
  val iter : (Warning_types.warning -> unit) -> t -> unit

  (** A printing function over the type [Warning_types.warning].  *)
  val print : Format.formatter -> Warning_types.warning -> unit
end
