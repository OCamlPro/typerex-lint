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

include TypedtreeIter

let iter_structure iterator cmt =
  let open Cmt_format in
  let module IA = (val iterator : IteratorArgument) in
  let module I = (MakeIterator(IA)) in
  match cmt.cmt_annots with
  | Implementation str ->
    I.iter_structure str
  | Packed _ -> ()
  | Interface sg -> ()
  | Partial_implementation _ ->
    failwith "Bad .cmt file"
  | Partial_interface _ ->
    failwith "Bad .cmti file"

let iter_signature iterator cmt =
  let open Cmt_format in
  let module IA = (val iterator : IteratorArgument) in
  let module I = (MakeIterator(IA)) in
  match cmt.cmt_annots with
  | Implementation str -> ()
  | Packed _ -> ()
  | Interface sg ->
    I.iter_signature sg
  | Partial_implementation _ ->
    failwith "Bad .cmt file"
  | Partial_interface _ ->
    failwith "Bad .cmti file"
