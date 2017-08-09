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

type file_content_type =
  | File_content
  | Warning_content of warning_info
  | Error_content of error_info

type file_content_value = {
  dom_content : Dom_html.element Js.t;
  mutable is_active : bool;
}

type file_content_data = {
  file_content_info :
    file_info;
  file_content_warnings_info :
    warning_info list;
  file_content_errors_info :
    error_info list;
  file_content_container :
    Dom_html.element Js.t;
  file_content_main_contents :
    (file_content_type, file_content_value) Hashtbl.t;
  file_content_creator :
    file_content_type -> Dom_html.element Js.t;
  file_content_warnings_filters :
    Web_filter_system.warnings_filter_system;
  file_content_errors_filters :
    Web_filter_system.errors_filter_system;
}

let create_file_content_data
      file_info
      file_warnings_info
      file_errors_info
      dom_content_container
      content_creator =
  {
    file_content_info = file_info;
    file_content_warnings_info = file_warnings_info;
    file_content_errors_info = file_errors_info;
    file_content_container = Tyxml_js.To_dom.of_element dom_content_container;
    file_content_main_contents = Hashtbl.create 64;
    file_content_creator = content_creator;
    file_content_warnings_filters = Web_filter_system.create ();
    file_content_errors_filters = Web_filter_system.create ();
  }

let active_file_content file_content_data =
  Hashtbl.fold begin fun _ content_value acc ->
    if content_value.is_active then
      match acc with
      | Some _ -> failwith "" (* todo web err *)
      | None -> Some content_value
    else
      acc
  end file_content_data.file_content_main_contents None

let focus_file_content file_content_data file_content_type =
  let file_content_warning =
    try
      Hashtbl.find
        file_content_data.file_content_main_contents
        file_content_type
    with
    | Not_found ->
       let default_content_value = {
         dom_content = file_content_data.file_content_creator file_content_type;
         is_active = false;
       }
       in
       Web_utils.dom_element_undisplay default_content_value.dom_content;
       Dom.appendChild
         file_content_data.file_content_container
         default_content_value.dom_content;
       Hashtbl.add
         file_content_data.file_content_main_contents
         file_content_type
         default_content_value;
       default_content_value
  in
  if not file_content_warning.is_active then begin
    let active = active_file_content file_content_data in
    begin match active with
      | Some content ->
         Web_utils.dom_element_undisplay content.dom_content;
         content.is_active <- false
      | _ -> ()
    end;
    Web_utils.dom_element_display file_content_warning.dom_content;
    file_content_warning.is_active <- true
  end
