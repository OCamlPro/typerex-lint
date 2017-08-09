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

open Lint_warning_types
open Lint_web_analysis_info

type error =
  | Unknow_warning_id of string
  | No_such_element_with_id of string
  | Ghost_location of Location.t
  | Active_navigation_element_is_not_unique
  | No_active_navigation_element
  | Get_value_of_empty_optional
  | Invalid_file_name of file_info
  | Invalid_content_attached_data of string
  | Open_warning_from_bad_file of warning_info * file_info
  | Open_error_from_bad_file of error_info * file_info
  | Active_main_file_content_is_not_unique of file_info

exception Web_exception of error

let log msg =
  Js_utils.js_error (Js.string msg)

let process_error exn =
  match exn with
  | Unknow_warning_id id ->
     log ("no warning entry with id '" ^ id ^ "' in this file")
  | No_such_element_with_id id ->
     log ("no element element found with id '" ^ id ^ "'")
  | Ghost_location loc ->
     let str_loc = "['loc']" in
     log ("location '" ^ str_loc ^ "' is not file-localizable")
  | Active_navigation_element_is_not_unique ->
     log "the active navigation element is not unique"
  | No_active_navigation_element ->
     log "there is no active navigation element"
  | Get_value_of_empty_optional ->
     log "trying to get the value of an empty optional"
  | Invalid_file_name {file_name; _} ->
     log ("'" ^ file_name ^ "' is not a valid file name")
  | Invalid_content_attached_data content_type ->
     log ("the content '" ^ content_type
          ^ "' is linked to data that must be normally not attached to it")
  | Open_warning_from_bad_file (warning, file) ->
     log ("trying to open a warning from '" ^ warning.warning_file.file_name
          ^ "' in file content of '" ^ file.file_name ^ "'")
  | Open_error_from_bad_file (error, file) ->
     log ("trying to open an error from '" ^ error.error_file.file_name
          ^ "' in file content of '" ^ file.file_name ^ "'")
  | Active_main_file_content_is_not_unique {file_name; _} ->
     log ("there is many active main content in file content '"
          ^ file_name ^ "'")
