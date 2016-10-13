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

let rec iter_tokens f filename =
  let ic = open_in filename in
  Location.input_name := filename;
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf filename;
  let rec iter f =
    let token = Lexer.token_with_comments lexbuf in
    let loc = Location.curr lexbuf in
    f token loc;
    match token with
    | Parser.EOF ->
      close_in ic
    | _ ->
      iter f
  in
  try
    iter f
  with e -> close_in ic; raise e

let get_tokens filename =
  let ic = open_in filename in
  Location.input_name := filename;
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf filename;
  let rec iter tokens =
    let token = Lexer.token_with_comments lexbuf in
    let loc = Location.curr lexbuf in
    match token with
    | Parser.EOF -> Array.of_list (List.rev tokens)
    | _ -> iter ((token, loc) :: tokens) in
  let tokens = iter [] in
  close_in ic;
  tokens
