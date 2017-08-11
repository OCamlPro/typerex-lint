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

(**
  The size of a line in the code viewer
  **)
val code_viewer_line_size :
  int

(**
  The number of context line in the code viewer
  **)
val code_viewer_context_line_number :
  int

(**
  Get the begin of the line with the context
  **)
val code_viewer_begin_context_from_line :
  int ->
  int

(**
  Get the end of the line with the context
  **)
val code_viewer_end_context_from_line :
  line_number:int ->
  lines_count:int ->
  int

(**
  Get a file code viewer
  **)
val file_code_viewer :
  Lint_web_analysis_info.file_info ->
  Html_types.iframe Tyxml_js.Html.elt

(**
  Get a warning code viewer
  **)
val warning_code_viewer :
  Lint_web_analysis_info.warning_info ->
  Html_types.iframe Tyxml_js.Html.elt

(* todo doc *)
val dropdown_simple_selection :
  'a ->
  string ->
  ('a -> Dom_html.element Js.t -> unit) ->
  Html_types.li Tyxml_js.Html.elt

(* todo doc *)
val dropdown_checkbox_selection :
  'a ->
  string ->
  ('a -> unit) ->
  ('a -> unit) ->
  Html_types.li Tyxml_js.Html.elt

(* todo doc *)
val dropdown_menu :
  string ->
  Html_types.ul_content_fun Tyxml_js.Html.elt list ->
  string list ->
  Html_types.div Tyxml_js.Html.elt
