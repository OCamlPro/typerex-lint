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

open Lint_utils

type error =
  | Plugin_already_registered of (module Lint_plugin_types.PLUGIN)
  | Plugin_not_found of (module Lint_plugin_types.PLUGIN)
  | Patch_file_not_found of string
  | Syntax_error of string

exception Plugin_error of error

let to_string = function
  | Plugin_already_registered plugin ->
    let module P = (val plugin : Lint_plugin_types.PLUGIN) in
    spf "Plugin '%s' is already registered." P.name
  | Plugin_not_found plugin ->
    let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
    spf "Plugin '%s' is not found." Plugin.name
  | Patch_file_not_found filename ->
    spf "Patch '%s' is not found." filename
  | Syntax_error filename ->
    spf "Syntax error in %S: cannot lint this file." filename

let print fmt err =
  Format.fprintf fmt "Plugin error: %s\n" (to_string err)
