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

type 'a filter_element = {
  filter_element_val : 'a;
  filter_element_dom : Dom_html.element Js.t;
}

type ('a, 'b) t = {
  filter_activated : ('b, 'a -> bool) Hashtbl.t;
  mutable filter_elements : 'a filter_element list;
}

type warning_filter_id =
  | Warning_type_filter of warning_info
  | Keyword_filter of string
  | Higher_severity_filter of int
  | File_filter of file_info

type warnings_filter_system = (warning_info, warning_filter_id) t

let create () =
  {
    filter_activated = Hashtbl.create 16;
    filter_elements = [];
  }

let register_element filter_system value dom =
  filter_system.filter_elements <-
    {
      filter_element_val = value;
      filter_element_dom = dom;
    } :: filter_system.filter_elements

let add_filter filter_system filter_id filter =
  Hashtbl.add filter_system.filter_activated filter_id filter

let remove_filter filter_system filter_id =
  Hashtbl.remove filter_system.filter_activated filter_id

let eval_filters filter_system =
  let full_filter value =
    Hashtbl.fold begin fun _ filter acc ->
      acc && filter value
    end filter_system.filter_activated true
  in
  List.iter begin fun filter_element ->
    if full_filter filter_element.filter_element_val then
      Web_utils.dom_element_display filter_element.filter_element_dom
    else
      Web_utils.dom_element_undisplay filter_element.filter_element_dom
  end filter_system.filter_elements

let value_of_warning_filter_id = function
  | Warning_type_filter warning ->
     begin fun warning_info ->
       not (
         String.equal
           warning.warning_type.decl.short_name
           warning_info.warning_type.decl.short_name
       )
     end
  | Keyword_filter kwd ->
     Web_utils.warning_contains_keyword kwd
  | File_filter file ->
     begin fun warning_info ->
       not (Web_utils.file_equals file warning_info.warning_file)
     end
  | Higher_severity_filter lvl ->
     begin fun warning_info ->
       warning_info.warning_type.decl.severity >= lvl
     end

let add_warning_filter filter_system filter_id =
  add_filter filter_system filter_id (value_of_warning_filter_id filter_id)
