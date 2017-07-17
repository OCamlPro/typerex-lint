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
open Lint_web
open Lint_warning_types
open Lint_web_analysis_info

let warning_content_code_view_header warning_info =
  div
    ~a:[
      a_class ["panel-heading"];
    ]
    [
      a
	~a:[
	  a_href (Web_utils.file_href warning_info.warning_file);
	]
	[pcdata warning_info.warning_file.file_name]
    ]

let warning_content_code_view_body warning_info =
  div
    ~a:[
      a_class ["panel-body"];
    ]
    [
      Web_code_viewer.warning_code_viewer warning_info;
    ]
    
let warning_content_code_view warning_info =
  div
    ~a:[
      a_class ["panel"; "panel-default"];
    ]
    [
      warning_content_code_view_header warning_info;
      warning_content_code_view_body warning_info;
    ]
  
let warning_content warning_info =
  let warning_desc =
    Printf.sprintf "Warning #%d :"
      warning_info.warning_id
  in
  let linter_desc =
    Printf.sprintf "%s : %s"
      warning_info.warning_linter.linter_name
      warning_info.warning_linter.linter_description
  in
  if Web_utils.warning_info_is_ghost warning_info then
    div
      [
	h3 [pcdata warning_desc];
	h3 [pcdata linter_desc];
      ]
  else
    div
      [
	h3 [pcdata warning_desc];
	h3 [pcdata linter_desc];
	br ();
	warning_content_code_view warning_info;
      ]
