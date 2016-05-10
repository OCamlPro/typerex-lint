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

val create :
  unit ->
  ((module Plugin_types.PLUGIN), (Input.input list) Globals.LintMap.t) Hashtbl.t

val add :
  ((module Plugin_types.PLUGIN), (Input.input list) Globals.LintMap.t) Hashtbl.t ->
  (module Plugin_types.PLUGIN) ->
  (Input.input list) Globals.LintMap.t ->
  unit

val find :
  ((module Plugin_types.PLUGIN), (Input.input list) Globals.LintMap.t) Hashtbl.t ->
  (module Plugin_types.PLUGIN) ->
  (Input.input list) Globals.LintMap.t

(** [iter_plugins f] applies f to all bindings in the global data structure
    [plugins]. [f] receives the [plugin] as first argument and the [LintMap.t]
    associated to this plugin as second. *)
val iter_plugins :
  ((module Plugin_types.PLUGIN) ->
   (Input.input list) Globals.LintMap.t -> unit) ->
    ((module Plugin_types.PLUGIN), (Input.input list) Globals.LintMap.t) Hashtbl.t ->
  unit
