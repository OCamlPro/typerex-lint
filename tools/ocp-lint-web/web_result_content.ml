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

let error_div error_info =
  let file_msg =
    a
      ~a:[
        a_class ["alert-link"];
      ]
      [
        pcdata error_info.error_file.file_name;
      ]
  in
  let error_msg =
    let str =
      match error_info.error_type with
      | Lint_db_types.Db_error e ->
         Lint_db_error.to_string e
      | Lint_db_types.Plugin_error e ->
         Lint_plugin_error.to_string e
      | Lint_db_types.Sempatch_error e ->
         e
      | Lint_db_types.Ocplint_error e ->
         e
    in
    pcdata str
  in
  div
    ~a:[
      a_class ["alert"; "alert-danger"];
    ]
    [
      error_div_head error_info;
      file_msg;
      br ();
      error_msg;
    ]

let warning_div_head warning_info =
  h4
    ~a:[
      a_class ["alert-heading"];
    ]
    [pcdata (Printf.sprintf "Warning #%d" warning_info.warning_id)]

let warning_div warning_info =
  let file_msg =
    a
      ~a:[
        a_class ["alert-link"];
      ]
      [
        pcdata warning_info.warning_file.file_name;
      ]
  in
  let line_msg =
    let str =
      let open Web_utils in
      match file_loc_of_warning_info warning_info with
      | Floc_line line ->
         Printf.sprintf "line %d" line
      | Floc_lines_cols (bline, _, eline, _) ->
         if bline = eline then
           Printf.sprintf "line %d" bline
         else
           Printf.sprintf "line %d to %d" bline eline
    in
    pcdata str
  in
  let linter_msg =
    pcdata (
      Printf.sprintf "raised from %s.%s"
        warning_info.warning_linter.linter_plugin.plugin_name
        warning_info.warning_linter.linter_name
    )
  in
  let div_warning =
    div
    ~a:[
      a_class ["alert"; "alert-warning"];
    ]
    [
      warning_div_head warning_info;
      pcdata "from ";
      file_msg;
      pcdata " ";
      line_msg;
      br ();
      b [pcdata "/!\\  "]; (* todo img *)
      pcdata warning_info.warning_type.decl.short_name;
      br ();
      linter_msg;
    ]
  in
  (Tyxml_js.To_dom.of_element div_warning)##onclick <- Dom_html.handler
  begin fun _ ->
    Web_navigation_system.open_warning_tab
      warning_info
      (Web_warning_content.warning_content warning_info);
    Js._true
  end;
  div_warning

let content warnings_info errors_info =
  div
    (List.map error_div errors_info
    @ List.map warning_div warnings_info)
