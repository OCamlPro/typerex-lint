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

(** [LINTARG] is a module type that is used by the functor
    [Lint_plugin_api.MakeLint]. *)
module type LINTARG = sig
  val name : string
  val version : string
  val short_name : string
  val details : string
  val enabled : bool
end

(** [LINTPATCHARG] is a module type that is used by the functor
    [Lint_plugin_api.MakeLintPatch]. *)
module type LINTPATCHARG = sig
  include LINTARG
  val patches : string list
end

module type LINT = sig
  val name : string
  val version : string
  val short_name : string
  val details : string
  val enabled : bool
  val inputs : Lint_input.input list
  val wdecls : Lint_warning_decl.WarningDeclaration.t
end
