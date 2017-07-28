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

open Tyxml_js.Html
open Lint_warning_types
open Lint_web_analysis_info


let error_div_head error_info =
  h4
    ~a:[
      a_class ["alert-heading"];
    ]
    [pcdata (Printf.sprintf "Error #%d" error_info.error_id)]

let error_div_body error_info =
  let file_msg = (* todo change *)
    span
      ~a:[
        a_class ["alert-link"];
      ]
      [
        pcdata error_info.error_file.file_name;
      ]
  in
  let description_msg =
    pcdata (
      Printf.sprintf "%s : %s"
        (Web_utils.error_type error_info)
        (Web_utils.error_description error_info)
    )
  in
  div
    [
      pcdata "from ";
      file_msg;
      br ();
      description_msg;
    ]

let error_div all_warnings_info all_errors_info error_info =
  let file_warnings_info =
    List.filter begin fun warning ->
      Web_utils.file_equals warning.warning_file error_info.error_file
    end all_warnings_info
  in
  let file_errors_info =
    List.filter begin fun error ->
      Web_utils.file_equals error.error_file error_info.error_file
    end all_errors_info
  in
  let div_error =
    div
      ~a:[
        a_class ["alert"; "alert-danger"];
      ]
      [
        error_div_head error_info;
        error_div_body error_info;
      ]
  in
  (Tyxml_js.To_dom.of_element div_error)##onclick <- Dom_html.handler
  begin fun _ ->
    let file_content_data =
      Web_file_content.open_tab
	error_info.error_file
	file_warnings_info
	file_errors_info
    in
    Web_file_content_data.focus_file_content
      file_content_data
      (Web_file_content_data.Error_content error_info)
    ;
    Js._true
  end;
  div_error

let errors_content analysis_info =
  div
    (List.map
       (error_div analysis_info.warnings_info analysis_info.errors_info)
       analysis_info.errors_info
    )

let errors_content_empty () =
  h3 [pcdata "There are no errors provided in this file"]

let content analysis_info =
  let content =
    if Web_utils.list_is_empty analysis_info.errors_info then
      errors_content_empty ()
    else
      errors_content analysis_info
  in
  div
    ~a:[
      a_class ["container"];
    ]
    [content]
