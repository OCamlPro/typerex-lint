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

module Linter = Plugin.MakeLint(struct
    let name = "Detect use of unwanted chars in files"
    let version = "1"
    let short_name = "not_that_char"
    let details =
      "Detect use of unwanted chars in files, such as tabs or non-ASCII chars"
    let enabled = true
  end)

type warning =
  | TabForIndent
  | NonAsciiChar of string

let w_tab_for_indent = Linter.new_warning
    ~id:1
    ~short_name:"tab_for_indent"
    ~msg:"Tabulations used for indentation"
    ~severity:10

let w_non_ascii_char = Linter.new_warning
    ~id:2
    ~short_name:"non_ascii_char"
    ~msg:"Non ASCII chars like '$char' in code"
    ~severity:3

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | TabForIndent ->
        w_tab_for_indent, []
      | NonAsciiChar s ->
        w_non_ascii_char, ["char", s]
  end)

let check_line lnum line file =

  let len = String.length line in
  let rec iter_indent not_found i =
    if i < len then
      match line.[i] with
      | '\t' ->
        if not_found then
          Warnings.report_file_line_col file lnum i TabForIndent;
        iter_indent false (i+1)
      | ' ' -> iter_indent not_found (i+1)
      | _ -> iter_code i

  and iter_code i =
    if i < len then
      let c = int_of_char line.[i] in
      if c < 32 || c > 127 then
        Warnings.report_file_line_col file lnum i
          (NonAsciiChar (Printf.sprintf "\\%03d" c)) (* warning and stop *)
      else
        iter_code (i+1)
  in
  iter_indent true 0

(* Later, we will use "ocplib-file":

let check_file file =
  FileString.iteri_lines (fun lnum line ->
      check_line lnum line file) file
*)

let check_file file =
  let ic = open_in file in
  let rec loop lnum ic =
    let line = input_line ic in
    check_line lnum line file;
    loop (lnum + 1) ic
  in
  try
    loop 1 ic
  with End_of_file ->
    close_in ic



(* Registering a main entry to the linter *)
module MainSRC = Linter.MakeInputML(struct
    let main source = check_file source
  end)
