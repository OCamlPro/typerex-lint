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
  The types of content in the alterable panel
  **)
type file_content_type =
  | File_content
  | Warning_content of Lint_web_analysis_info.warning_info
  | Error_content of Lint_web_analysis_info.error_info

(**
  The required informations on alterable panel content
  **)
type file_content_value = {
  (** The dom element of the content **)
  dom_content : Dom_html.element Js.t;
  (** true if the content is the displayed content, else false **)
  mutable is_active : bool;
}

(**
  The file content datas
  **)
type t = {
  (** The file informations of the file content **)
  file_content_info :
    Lint_web_analysis_info.file_info;
  (** The warnings raised on this file **)
  file_content_warnings_info :
    Lint_web_analysis_info.warning_info list;
  (** The errors raised on this file **)
  file_content_errors_info :
    Lint_web_analysis_info.error_info list;
  (** The alterable panel **)
  file_content_container :
    Dom_html.element Js.t;
  (** The alterable panel contents informations **)
  file_content_main_contents :
    (file_content_type, file_content_value) Hashtbl.t;
  (** The alterable panel content creator **)
  file_content_panel_creator :
    file_content_type -> Html_types.div Tyxml_js.Html.elt;
  (** The warnings filter system **)
  file_content_warnings_filters :
    Web_filter_system.warnings_filter_system;
  (** The errors filter system **)
  file_content_errors_filters :
    Web_filter_system.errors_filter_system;
}

(**
  Create file content data
  **)
val create :
  Lint_web_analysis_info.file_info ->
  Lint_web_analysis_info.warning_info list ->
  Lint_web_analysis_info.error_info list ->
  [> Html_types.div ] Tyxml_js.Html.elt ->
  (file_content_type -> Html_types.div Tyxml_js.Html.elt) ->
  t

(**
  Get the displayed content type of the alterable panel
  The option is empty if the panel is no content was created
  Raise Web_exception Active_main_file_content_is_not_unique if there are more than one panel marked as active
  **)
val focused_panel_content :
  t ->
  file_content_type option

(**
  Display the content type in the alterable panel
  **)
val focus_file_content :
  t ->
  file_content_type ->
  unit
