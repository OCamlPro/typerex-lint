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
open Lint_web_warning
open Lint_web_plugin
       
let warning_content_code_view_header warning_entry =
  div
    ~a:[
      a_class ["panel-heading"];
    ]
    [
      a
	~a:[
	  a_href (Web_utils.warning_href warning_entry);
	]
	[pcdata warning_entry.warning_file_name]
    ]

let warning_content_code_view_body warning_entry =
  div
    ~a:[
      a_class ["panel-body"];
    ]
    [
      Web_code_viewer.warning_code_viewer warning_entry;
    ]
    
let warning_content_code_view warning_entry =
  div
    ~a:[
      a_class ["panel"; "panel-default"];
    ]
    [
      warning_content_code_view_header warning_entry;
      warning_content_code_view_body warning_entry;
    ]
  
let warning_content warning_entry plugin_entry =
  let warning_desc =
    "Warning " ^ (string_of_int warning_entry.warning_result.decl.id) ^ " :"
  in
  let linter_desc =
    plugin_entry.plugin_entry_linter_name
    ^ " : "
    ^ plugin_entry.plugin_entry_linter_description
  in
  div
    [
      h3 [pcdata warning_desc];
      h3 [pcdata linter_desc];
      br ();
      warning_content_code_view warning_entry;
    ]

