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

open SimpleConfig (* for !! *)
open Plugin_text

module Linter = PluginText.MakeLint(struct
    let name = "Useless space character and empty line at the end of file."
    let short_name = "useless_space_line"
    let details = "Checks useless space character at the end  \
                   of lines and empty line at the end of files."
    let enable = true
  end)

type warning =
  | Useless_space of int
  | Useless_line of string

let w_useless_space = Linter.new_warning
    ~id:1
    ~short_name:"useless_space"
    ~msg:"Useless space character at line $line."

let w_useless_line = Linter.new_warning
    ~id:2
    ~short_name:"useless_line"
    ~msg:"Useless empty line at the end of '$file'."

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | Useless_space line ->
        w_useless_space, ["line", string_of_int line]
      | Useless_line file ->
        w_useless_line, ["file", file]
  end)

let check_line lnum line file =
  let open Location in
  let open Lexing in
  let len = String.length line in
  let pos = { Lexing.dummy_pos with pos_fname = file; pos_lnum = lnum } in
  let loc = { Location.none with loc_start = pos; loc_end = pos} in
  if len > 0 && line.[len - 1] = ' ' then
    Warnings.report loc (Useless_space lnum)

let check_file file =
  let ic = open_in file in
  let last = ref "" in
  let rec loop lnum ic =
    try
      last := input_line ic;
      check_line lnum !last file;
      loop (lnum + 1) ic
    with End_of_file ->
      if !last = "" then Warnings.report_file file (Useless_line file) in
  loop 1 ic;
  close_in ic


(* Registering a main entry to the linter *)
module MainSRC = Linter.MakeInputML(struct
    let main source = check_file source
  end)
