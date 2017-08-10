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
  The differents web errors
  **)
type error =
  | Unknow_warning_id of
      string
  | No_such_element_with_id of
      string
  | Ghost_location of
      Location.t
  | Active_navigation_element_is_not_unique
  | No_active_navigation_element
  | Get_value_of_empty_optional
  | Invalid_file_name of
      Lint_web_analysis_info.file_info
  | Invalid_content_attached_data of
      string
  | Open_warning_from_bad_file of
      Lint_web_analysis_info.warning_info * Lint_web_analysis_info.file_info
  | Open_error_from_bad_file of
      Lint_web_analysis_info.error_info * Lint_web_analysis_info.file_info
  | Active_main_file_content_is_not_unique of
      Lint_web_analysis_info.file_info

exception Web_exception of error

(**
  Process a web error
  **)
val process_error :
  error ->
  unit
