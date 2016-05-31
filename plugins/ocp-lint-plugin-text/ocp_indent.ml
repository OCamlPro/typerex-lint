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

module OCPIndent = PluginText.MakeLint(struct
    let name = "Indention with ocp-indent"
    let short_name = "ocp_indent"
    let details = "Checks indentation with ocp-indent"
    let enable = true
  end)

let w_check_indent = OCPIndent.new_warning
    [ Lint_warning.kind_code ]
    ~id:1
    ~short_name:"ocp_indent_check"
    ~msg:"File '$file' is not indented correctly."

module Warnings = OCPIndent.MakeWarnings (
  struct
    include Lint_warning_types.DefaultWarning(struct type t = string end)

    let report loc file = w_check_indent loc ["file", file]
  end)

let check_indent file =
  let ic = open_in file in
  let text_init = Buffer.create 42 in
  let text_indented = Buffer.create 42 in
  let nstream = Nstream.of_channel ic in
  let block = IndentBlock.empty in
  let rec loop ic =
    try
      Buffer.add_string text_init (input_line ic ^ "\n");
      loop ic
    with End_of_file -> () in
  loop ic;
  seek_in ic 0;
  let output = {
    IndentPrinter.
    debug = false;
    config = IndentConfig.default;
    in_lines = (fun _ -> true);
    indent_empty = false;
    adaptive = true;
    kind = IndentPrinter.Print (fun s () -> Buffer.add_string text_indented s)
  } in

  (* Getting indented code by ocp-indent. *)
  IndentPrinter.(proceed output nstream block ());

  if Buffer.contents text_init <> Buffer.contents text_indented then
    let pos = Lexing.({dummy_pos with pos_fname = file}) in
    let loc = Location.({none with loc_start = pos}) in
    Warnings.report loc file


(* Registering a main entry to the linter *)
module MainSRC = OCPIndent.MakeInputML(struct
    let main source = check_indent source
  end)
