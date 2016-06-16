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
  | File_not_found of string
  | File_not_in_db of string
  | Plugin_not_in_db of string * string
  | Linter_not_in_db of string * string * string
  | No_db_found

let to_string = function
  | No_db_found ->
    Printf.sprintf
      "No DB file found, you should use --init option to use DB features."
  | File_not_found filename ->
    Printf.sprintf
      "Ignoring warnings on %S. This file may be generated." filename
  | File_not_in_db filename ->
    Printf.sprintf
      "Ignoring warnings on %S. This file is not in the db." filename
  | Plugin_not_in_db (fname, pname) ->
    Printf.sprintf
      "Ignoring warnings on %S. The plugin %s is not in the db." fname pname
  | Linter_not_in_db  (fname, pname, lname) ->
    Printf.sprintf
      "Ignoring warnings on %S. The linter %s.%s is not in the db."
      fname
      pname
      lname

let print fmt err =
  Format.fprintf fmt "Db error: %s\n" (to_string err)

exception Db_error of error

let () =
  Printexc.register_printer (fun exn ->
      match exn with
      | Db_error error ->
        Some (Printf.sprintf "Db_error %s" (to_string error))
      | _ -> None
    )
