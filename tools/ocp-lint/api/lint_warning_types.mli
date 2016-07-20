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

type substitution = (string * string) list

type warning = {
  loc : Location.t;    (* The location of the warning *)
  decl : warning_declaration;
  output: string;
}

(*
Severity is an int between 1 - 10:
  [1 - 2]  : no impact on the acceptance of the PR, small readabily problems
             (ex: parenteses, ident too short, etc.)
  [3 - 5]  : issues that should be discuss but the PR is still acceptable
  [6 - 10] : discard PR (indentation, 80 char, perf etc.)
*)


and warning_declaration = {
  short_name : string; (* A short name to identify a warning *)
  message : string;    (* The displayed message *)
  id : int;            (* Warning number *)
  severity : int;      (* Severity of the warning (1 - 10) *)
}

module type WARNINGARG = sig
  type t
  val to_warning : t -> warning_declaration * substitution
end
