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

type input =
  | InStruct of (Parsetree.structure -> unit)
  | InInterf of (Parsetree.signature -> unit)
  | InTop of (Parsetree.toplevel_phrase -> unit)
  | InCmt of (Cmt_format.cmt_infos -> unit)
  | InMl of (string -> unit)
  | InMli of (string -> unit)
  | InAll of (string list -> unit)
  | InTokens of (string -> unit)

module type INPUT = sig val input : input end
module type STRUCTURE = sig val main : Parsetree.structure -> unit end
module type INTERFACE = sig val main : Parsetree.signature -> unit end
module type TOPLEVEL = sig val main : Parsetree.toplevel_phrase -> unit end
module type CMT = sig val main : Cmt_format.cmt_infos -> unit end
module type ML = sig val main : string -> unit end
module type MLI = sig val main : string -> unit end
module type ALL = sig val main : string list -> unit end
module type TOKENS = sig
  val main : string -> unit end
