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
  The types of content
  **)
type navigation_element =
  | HomeElement
  | WarningsElement
  | ErrorsElement
  | FileElement of Lint_web_analysis_info.file_info

(**
  The attached data of the contents
 **)
type navigation_attached_data =
  | No_attached_data
  | File_content_attached_data of Web_file_content_data.t

(**
  The informations on a navigation element
  **)
type navigation_value = {
  (** The tab dom element **)
  dom_tab : Dom_html.element Js.t;
  (** The content dom element **)
  dom_content : Dom_html.element Js.t;
  (** true if the tab is open, else false **)
  mutable tab_is_open : bool;
  (** true if the content is created, else false **)
  mutable content_is_created : bool;
  (** The attached datas **)
  attached_data : navigation_attached_data
}

module NavigationElement :
sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
end

(**
  The navigation system
  **)
type t = {
  (** The navigations elements **)
  navigation_elements :
    navigation_value Hashtbl.Make(NavigationElement).t;
  (** The dom container for tabs **)
  navigation_dom_tabs :
    Dom_html.element Js.t;
  (** The dom container for contents **)
  navigation_dom_contents :
    Dom_html.element Js.t;
  (** The content creator **)
  navigation_content_creator :
    t ->
    navigation_element ->
    Dom_html.element Js.t * navigation_attached_data;
}

(**
  Create navigation system
  **)
val create :
  (
    t ->
    navigation_element ->
    Dom_html.element Js.t * navigation_attached_data
  ) ->
  t

(**
  Initialize the navigation system
  **)
val init :
  t ->
  unit

(**
  Open the home tab
  **)
val open_home_tab :
  t ->
  unit

(**
  Open the warnings tab
  **)
val open_warnings_tab :
  t ->
  unit

(**
  Open the errors tab
  **)
val open_errors_tab :
  t ->
  unit

(**
  Open the file tab
  **)
val open_file_tab :
  t ->
  Lint_web_analysis_info.file_info ->
  Web_file_content_data.t
