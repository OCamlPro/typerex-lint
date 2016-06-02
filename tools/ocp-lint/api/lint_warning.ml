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

open Lint_warning_types

module Warning = struct

  let add_warning pname lname warning =
    Lint_db.DefaultDB.update pname lname warning

  let add pname lname loc decl output =
    let warning = {loc; decl; output} in
    add_warning pname lname warning
end

let cmp_loc loc1 loc2 =
  let pos_lnum1 = loc1.Lexing.pos_lnum in
  let pos_bol1 = loc1.Lexing.pos_bol in
  let pos_cnum1 = loc1.Lexing.pos_cnum in

  let pos_lnum2 = loc2.Lexing.pos_lnum in
  let pos_bol2 = loc2.Lexing.pos_bol in
  let pos_cnum2 = loc2.Lexing.pos_cnum in

  pos_lnum1 = pos_lnum2 &&
  pos_bol1 = pos_bol2 &&
  pos_cnum1 = pos_cnum2

let cmp_warnings w1 w2 =
  let open Lint_warning_types in
  let file1 = Filename.basename w1.loc.Location.loc_start.Lexing.pos_fname in
  let file2 = Filename.basename w2.loc.Location.loc_start.Lexing.pos_fname in

  let s_loc1 = w1.loc.Location.loc_start in
  let s_loc2 = w2.loc.Location.loc_start in

  let e_loc1 = w1.loc.Location.loc_end in
  let e_loc2 = w2.loc.Location.loc_end in

  w1.decl.short_name = w2.decl.short_name &&
  w1.decl.id = w2.decl.id &&
  file1 = file2 &&
  cmp_loc s_loc1 s_loc2 &&
  cmp_loc e_loc1 e_loc2
