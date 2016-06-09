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

module Linter = Plugin_tokens.Plugin.MakeLint(struct
    let name = "Detect use of unwanted chars in files"
    let version = 1
    let short_name = "not_that_char"
    let details =
      "Detect use of unwanted chars in files, such as tabs or non-ASCII chars"
    let enable = true
  end)

type warning =
  | NonAsciiCharInComment
  | NonAsciiCharInIdent of string
  | NonAsciiCharInLabel of string
  | NonAsciiCharInString

let w_non_ascii_char_in_comment = Linter.new_warning
    ~id:2
    ~short_name:"non_ascii_char.comment"
    ~msg:"Non ASCII chars in comment"

let w_non_ascii_char_in_ident = Linter.new_warning
    ~id:2
    ~short_name:"non_ascii_char.ident"
    ~msg:"Non ASCII chars in ident '$ident'"

let w_non_ascii_char_in_label = Linter.new_warning
    ~id:2
    ~short_name:"non_ascii_char.label"
    ~msg:"Non ASCII chars in label '$label'"

let w_non_ascii_char_in_string = Linter.new_warning
    ~id:2
    ~short_name:"non_ascii_char.string"
    ~msg:"Non ASCII chars in string"

module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | NonAsciiCharInComment ->
        w_non_ascii_char_in_comment, []
      | NonAsciiCharInIdent s ->
        w_non_ascii_char_in_ident, ["ident", s]
      | NonAsciiCharInLabel s ->
        w_non_ascii_char_in_label, ["label", s]
      | NonAsciiCharInString ->
        w_non_ascii_char_in_string, []
  end)

let check line =

  let len = String.length line in
  let rec iter i =
    i < len &&
    let c = int_of_char line.[i] in
    c < 32 || c > 127 ||
    iter (i+1)
  in
  iter 0

let check_tokens tokens =
  Array.iter (fun (token, loc) ->
      match token with
      | Parser.LIDENT s | Parser.UIDENT s ->
        if check s then
          Warnings.report loc (NonAsciiCharInIdent (String.escaped s))
      | Parser.COMMENT (s, _loc) ->
        if check s then
          Warnings.report loc NonAsciiCharInComment
      | Parser.LABEL s | Parser.OPTLABEL s ->
        if check s then
          Warnings.report loc (NonAsciiCharInLabel (String.escaped s))
      | Parser.STRING (s, _) ->
        if check s then
          Warnings.report loc NonAsciiCharInString
      | _ -> ()
    ) tokens

(* Registering a main entry to the linter *)
module MainSRC = Linter.MakeInputTokens(struct
    let main source = check_tokens source
  end)
