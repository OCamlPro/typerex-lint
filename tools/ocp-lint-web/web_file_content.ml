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

let file_code_viewer_header file_info =
  div
    ~a:[
      a_class ["panel-heading"];
    ]
    [
      a
       ~a:[
          a_href (Web_utils.file_href file_info);
        ]
        [pcdata file_info.file_name]
    ]

let file_code_viewer_body file_info =
  div
    ~a:[
      a_class ["panel-body"];
    ]
    [
      Web_code_viewer.file_code_viewer file_info;
    ]

let file_code_viewer file_info =
  div
    ~a:[
      a_class ["panel"; "panel-default"];
    ]
    [
      file_code_viewer_header file_info;
      file_code_viewer_body file_info;
    ]

let content_body_file active file_info =
  let classes =
    if active then
      ["tab-pane"; "fade"; "in"; "active"]
    else
      ["tab-pane"; "fade"; "in"]
  in
  div
    ~a:[
      a_id ("file-content-full-" ^ (Digest.to_hex file_info.file_hash));
      a_class classes;
    ]
    [
      file_code_viewer file_info;
    ]

let content_body_warning active warning_info =
  let classes =
    if active then
      ["tab-pane"; "fade"; "in"; "active"]
    else
      ["tab-pane"; "fade"; "in"]
  in
  div
    ~a:[
      a_id ("file-content-warning-" ^ (string_of_int warning_info.warning_id));
      a_class classes
    ]
    [
      Web_warning_content.warning_content warning_info;
    ]

let content_menu file_info warnings_info =
  let li_element href content =
    li
      [
        a
          ~a:[
            a_user_data "toggle" "tab";
            a_href href;
          ]
          [pcdata content]
      ]
  in
  ul
    (
      li_element
        ("#file-content-full-" ^ (Digest.to_hex file_info.file_hash))
        "Full file" ::
      List.map begin fun warning ->
       let href =
         "#file-content-warning-" ^ (string_of_int warning.warning_id)
       in
       let content =
         Printf.sprintf "Warning %d : %s"
           warning.warning_id
           warning.warning_type.decl.short_name
        in
        li_element href content
      end warnings_info
    )

let all_file_content_body file_info warnings_info =
  div
    ~a:[
      a_class ["tab-content"];
    ]
    (
      content_body_file true file_info ::
      List.map (content_body_warning false) warnings_info
    )

let all_file_content file_info warnings_info =
  div
    [
      content_menu file_info warnings_info;
      all_file_content_body file_info warnings_info;
    ]

let warning_content_body warning_info others_warnings_info =
  div
    ~a:[
      a_class ["tab-content"];
    ]
    (
      content_body_file false warning_info.warning_file ::
      content_body_warning true warning_info ::
      List.map (content_body_warning false) others_warnings_info
    )

let warning_content warning_info all_file_warnings_info =
  let others_warnings_info =
    List.filter begin fun warning ->
      warning.warning_id != warning_info.warning_id
    end all_file_warnings_info
  in
  div
    [
      content_menu warning_info.warning_file all_file_warnings_info;
      warning_content_body warning_info others_warnings_info;
    ]
