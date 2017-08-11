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
  The informations on one element for filter system
  **)
type 'a filter_element = {
  (** The value to filter **)
  filter_element_val : 'a;
  (** The attached dom element of the value **)
  filter_element_dom : Dom_html.element Js.t;
}

(**
  The filter system
  The 'a type corresponds to the type of the values to filters
  The 'b type corresponds to the filters identifiers
  **)
type ('a, 'b) t = {
  (** The table of the activated filters **)
  filter_activated : ('b, 'a -> bool) Hashtbl.t;
  (** The list of values to filter **)
  mutable filter_elements : 'a filter_element list;
}

(**
  Create a generic filter system
  **)
val create :
  unit ->
  ('a, 'b) t

(**
  Add a value to filter into a generic filter system
  **)
val register_element :
  ('a, 'b) t ->
  'a ->
  Dom_html.element Js.t ->
  unit

(**
  Add a filter in a generic filter system
  **)
val add_filter :
  ('a, 'b) t ->
  'b ->
  ('a -> bool) ->
  unit

(**
  Remove a filter in a generic filter system
  **)
val remove_filter :
  ('a, 'b) t ->
  'b ->
  unit

(**
  Evaluates the filters on the registered values
  Only the dom elements of values that respect the filter are displayed
  **)
val eval_filters :
  ('a, 'b) t ->
  unit

(**
  The types of filters on a warning
  **)
type warning_filter_id =
  | Warning_type_filter of Lint_web_analysis_info.warning_info
  | Warning_keyword_filter of string
  | Warning_higher_severity_filter of int
  | Warning_file_filter of Lint_web_analysis_info.file_info

(**
  The filter system for warnings
  **)
type warnings_filter_system =
    (Lint_web_analysis_info.warning_info, warning_filter_id) t

(**
  Get the predicate of a warning filter id
  **)
val value_of_warning_filter :
  warning_filter_id ->
  Lint_web_analysis_info.warning_info ->
  bool

(**
  Add a filter in a warnings filter system
  **)
val add_warning_filter :
  warnings_filter_system ->
  warning_filter_id ->
  unit

(**
  The types of filters on an error
  **)
type error_filter_id =
  | Error_type_filter of Lint_web_analysis_info.error_info
  | Error_keyword_filter of string
  | Error_file_filter of Lint_web_analysis_info.file_info

(**
  The filter system for errors
  **)
type errors_filter_system =
    (Lint_web_analysis_info.error_info, error_filter_id) t

(**
  Get the predicate of an error filter id
  **)
val value_of_error_filter :
  error_filter_id ->
  Lint_web_analysis_info.error_info ->
  bool

(**
  Add a filter in a warnings filter system
  **)
val add_error_filter :
  errors_filter_system ->
  error_filter_id ->
  unit
