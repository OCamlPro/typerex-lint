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

type error =
  | Plugin_already_registered of (module Lint_plugin_types.PLUGIN)
  | Linter_already_registered of string
  | Plugin_not_found of (module Lint_plugin_types.PLUGIN)
  | Patch_file_not_found of string
  | Syntax_error of string
  | Plugin_exception of exn

exception Plugin_error of error

let to_string = function
  | Plugin_already_registered plugin ->
    let module P = (val plugin : Lint_plugin_types.PLUGIN) in
    Printf.sprintf "Plugin '%s' is already registered." P.short_name
  | Linter_already_registered lname ->
    Printf.sprintf "Linter '%s' is already registered." lname
  | Plugin_not_found plugin ->
    let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
    Printf.sprintf "Plugin '%s' is not found." Plugin.name
  | Patch_file_not_found filename ->
    Printf.sprintf "Patch '%s' is not found." filename
  | Syntax_error filename ->
    Printf.sprintf "Syntax error in %S: cannot lint this file." filename
  | Plugin_exception exn ->
    Printf.sprintf "exception %s." (Printexc.to_string exn)

let print fmt err =
  Format.fprintf fmt "Plugin error: %s\n" (to_string err)
