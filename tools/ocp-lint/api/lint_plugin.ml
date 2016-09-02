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

type t =
  ((module Lint_plugin_types.PLUGIN), (module Lint_types.LINT) Lint_map.t)
    Hashtbl.t

let create () = Hashtbl.create 42

let add = Hashtbl.replace

let mem = Hashtbl.mem

let find = Hashtbl.find

let exist_name plugins short_name =
  let res = ref false in
  Hashtbl.iter (fun p _ ->
      let module Plugin = (val p : Lint_plugin_types.PLUGIN) in
      let p_short_name = Plugin.short_name in
      if p_short_name = short_name then res := true)
    plugins;
  !res

let check_uniqueness plugins plugin =
  let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
  let short_name = Plugin.short_name in
  not (mem plugins plugin) && not (exist_name plugins short_name)

let iter_plugins apply plugins = Hashtbl.iter apply plugins
