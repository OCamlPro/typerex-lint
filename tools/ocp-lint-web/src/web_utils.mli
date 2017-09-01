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
  A location in a file
  **)
type file_loc =
  | Floc_line of int
  | Floc_lines_cols of int * int * int * int

(**
  Get the file location of a warning
  Raise Web_exception Ghost_location if the location of the warning is ghost
  **)
val file_loc_of_warning_info :
  Lint_web_analysis_info.warning_info ->
  file_loc

(**
  true if the list if empty, else false
  **)
val list_is_empty :
  'a list ->
  bool

(**
  true if the string contains the keyword, else false
  **)
val string_contains_keyword :
  kwd:string ->
  str:string ->
  bool

(**
  Get the string writing with at most n characters
  **)
val string_overflow :
  int ->
  string ->
  string

(**
  Get the value of a non-empty optional
  Raise Web_exception Get_value_of_empty_optional if the optional is empty
  **)
val value_of_optional :
  'a option ->
  'a

(**
  Get the value of a non-empty js optional
  Raise Web_exception Get_value_of_empty_optional if the optional is empty
  **)
val value_of_js_option :
  'a Js.Opt.t ->
  'a

(**
  Create an html empty node
  **)
val html_empty_node :
  unit ->
  [> `PCDATA ] Tyxml_js.Html.elt

(**
  Remove a node from the dom
  **)
val dom_node_remove :
  Dom.node Js.t ->
  unit

(**
  true if the element is visible, else false
  **)
val dom_element_is_display :
  Dom_html.element Js.t ->
  bool

(**
  Make visible the element
  **)
val dom_element_display :
  Dom_html.element Js.t ->
  unit

(**
  Make invisible the element
  **)
val dom_element_undisplay :
  Dom_html.element Js.t ->
  unit

(**
  Get a dom element by id
  Raise Web_exception No_such_element_with_id if there is no element with specified id
  **)
val get_element_by_id :
  string ->
  'a Tyxml_js.Html5.elt

(**
  Get the JSON of the variable from his name
  **)
val json_from_js_var :
  string ->
  Yojson.Basic.json

(**
  Get the URL for the all file code viewer
  **)
val file_href :
  Lint_web_analysis_info.file_info ->
  string

(**
  Get the URL for the warning code viewer
  **)
val file_warning_href :
  Lint_web_analysis_info.warning_info ->
  string

(**
  Test if the files are the same
  **)
val file_equals :
  Lint_web_analysis_info.file_info ->
  Lint_web_analysis_info.file_info ->
  bool

(**
  Compare the files
  **)
val file_compare :
  Lint_web_analysis_info.file_info ->
  Lint_web_analysis_info.file_info ->
  int

(**
  Get the name of the file without the path
  Raise Web_exception Invalid_file_name file_info if the filename is not valid
  **)
val file_short_name :
  Lint_web_analysis_info.file_info ->
  string

(**
  Get the filename writing with at most n characters
  **)
val filename_overflow :
  int ->
  string ->
  string

(**
  Get the list of files without duplicates
  **)
val files_set :
  Lint_web_analysis_info.file_info list ->
  Lint_web_analysis_info.file_info list

(**
  Test if the plugins are the same
  **)
val plugin_equals :
  Lint_web_analysis_info.plugin_info ->
  Lint_web_analysis_info.plugin_info ->
  bool

(**
  Compare the plugins
  **)
val plugin_compare :
  Lint_web_analysis_info.plugin_info ->
  Lint_web_analysis_info.plugin_info ->
  int

(**
  Test if the linters are the same
  **)
val linter_equals :
  Lint_web_analysis_info.linter_info ->
  Lint_web_analysis_info.linter_info ->
  bool

(**
  Compare the linters
  **)
val linter_compare :
  Lint_web_analysis_info.linter_info ->
  Lint_web_analysis_info.linter_info ->
  int

(**
  Get the full name of the linter
  **)
val linter_name :
  pname:string ->
  lname:string ->
  string

(**
  Test if the warnings are the same
  **)
val warning_equals :
  Lint_web_analysis_info.warning_info ->
  Lint_web_analysis_info.warning_info ->
  bool

(**
  Compare the warnings
  **)
val warning_compare :
  Lint_web_analysis_info.warning_info ->
  Lint_web_analysis_info.warning_info ->
  int

(**
  Get the full name of the warning
  **)
val warning_name :
  Lint_web_analysis_info.warning_info ->
  string

(**
  true if warning is ghost, else false
  **)
val warning_is_ghost :
  Lint_web_analysis_info.warning_info ->
  bool

(**
  Get the list of warnings without duplicates
  **)
val warnings_set :
  Lint_web_analysis_info.warning_info list ->
  Lint_web_analysis_info.warning_info list

(**
  Get the type of the error
  **)
val error_type :
  Lint_web_analysis_info.error_info ->
  string

(**
  Get the description of the error
  **)
val error_description :
  Lint_web_analysis_info.error_info ->
  string

(**
  Test if the errors are the same
  **)
val error_equals :
  Lint_web_analysis_info.error_info ->
  Lint_web_analysis_info.error_info ->
  bool

(**
  Compare the errors
  **)
val error_compare :
  Lint_web_analysis_info.error_info ->
  Lint_web_analysis_info.error_info ->
  int

(**
  Get the list of errors without duplicates
  **)
val errors_set :
  Lint_web_analysis_info.error_info list ->
  Lint_web_analysis_info.error_info list
