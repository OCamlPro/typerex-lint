(**************************************************************************)
(*                                                                        *)
(*                              OCamlPro TypeRex                          *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*      (GNU Public Licence version 3.0).                                 *)
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

open Asttypes
open Parsetree
open Ast_mapper
open Check_types
open Configuration
open Info

let info = {
  name = "Missing Interface";
  details = "Long details";
  cat = Interface;
}

let run config reports sources =
  let mlis =
    List.filter (fun file -> Filename.check_suffix file "mli") sources in
  let mlis = List.map Filename.chop_extension mlis in
  List.iter (fun file ->
      let name = Filename.chop_extension file in
      if not (List.mem name mlis) then
        let msg =
          Printf.sprintf "Missing interface for %S" file in
        Reports.add (Reports.warning Location.none info msg) reports)
    (List.filter (fun file -> Filename.check_suffix file "ml") sources)

let check : Check_types.global_check = { global_run = run; global_info = info }
