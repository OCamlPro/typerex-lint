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
  A fold menu
  **)
type hideable_menu = {
  (** The title of the menu **)
  hideable_menu_title : Dom_html.element Js.t;
  (** The icon of the menu (close or open) **)
  hideable_menu_icon : Dom_html.element Js.t;
  (** The content of the menu **)
  hideable_menu_content : Dom_html.element Js.t;
  (** true if the menu is unfolded, else false **)
  mutable hideable_menu_is_open : bool;
}

(**
  Create a hideable menu
  The menu is initially closed
  **)
val hideable_menu_create :
  string ->
  Html_types.div Tyxml_js.Html.elt ->
  hideable_menu

(**
  true if the menu is open, else false
  **)
val hideable_menu_is_open :
  hideable_menu ->
  bool

(**
  Unfold the content of the menu
  **)
val hideable_menu_open :
  hideable_menu ->
  unit

(**
  Fold the content of the menu
  **)
val hideable_menu_close :
  hideable_menu ->
  unit

(**
  Get the HTML representation of the menu
  **)
val hideable_menu_div_element :
  hideable_menu ->
  [> Html_types.div ] Tyxml_js.Html.elt

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

(**
  Create a simple selection for dropdown menu
  **)
val dropdown_simple_selection :
  'a ->
  string ->
  ('a -> Dom_html.element Js.t -> unit) ->
  Html_types.li Tyxml_js.Html.elt

(**
  Create a checkbox selection for dropdown menu
  **)
val dropdown_checkbox_selection :
  'a ->
  string ->
  ('a -> unit) ->
  ('a -> unit) ->
  Html_types.li Tyxml_js.Html.elt

(**
  Create a dropdown menu
  **)
val dropdown_menu :
  string ->
  Html_types.ul_content_fun Tyxml_js.Html.elt list ->
  string list ->
  Html_types.div Tyxml_js.Html.elt

(**
  Create a searchbox input
  **)
val searchbox :
  ('a, 'b) Web_filter_system.t ->
  (string -> 'b) ->
  ('b -> 'a -> bool) ->
  string list ->
  Html_types.div Tyxml_js.Html.elt

(**
  Create a warning box
  **)
val warning_box :
  Lint_web_analysis_info.warning_info ->
  (unit -> unit) ->
  [> Html_types.div ] Tyxml_js.Html.elt

(**
  Create an error box
  **)
val error_box :
  Lint_web_analysis_info.error_info ->
  (unit -> unit) ->
  [> Html_types.div ] Tyxml_js.Html.elt
