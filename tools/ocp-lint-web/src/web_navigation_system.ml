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
open Web_errors

type navigation_element =
  | HomeElement
  | WarningsElement
  | ErrorsElement
  | FileElement of file_info

type navigation_attached_data =
  | No_attached_data
  | File_content_attached_data of Web_file_content_data.t

type navigation_value = {
  dom_tab : Dom_html.element Js.t;
  dom_content : Dom_html.element Js.t;
  mutable tab_is_open : bool;
  mutable content_is_created : bool;
  attached_data : navigation_attached_data
}

module NavigationElement =
  struct
    type t = navigation_element
    let equal x y =
      match x,y with
      | HomeElement, HomeElement -> true
      | HomeElement, _ -> false
      | WarningsElement, WarningsElement -> true
      | WarningsElement, _ -> false
      | ErrorsElement, ErrorsElement -> true
      | ErrorsElement, _ -> false
      | FileElement f, FileElement f' -> Web_utils.file_equals f f'
      | FileElement _, _ -> false
    let hash = Hashtbl.hash
  end

module NavigationElementHashtbl = Hashtbl.Make(NavigationElement)


type  t = {
  navigation_elements :
    navigation_value NavigationElementHashtbl.t;
  navigation_dom_tabs :
    Dom_html.element Js.t;
  navigation_dom_contents :
    Dom_html.element Js.t;
  navigation_content_creator :
    t ->
    navigation_element ->
    Html_types.div Tyxml_js.Html.elt * navigation_attached_data;
}

let navigation_value_is_active nav_val =
  Js.to_bool (nav_val.dom_tab##classList##contains (Js.string "active"))

let navigation_value_set_active nav_val =
  nav_val.dom_tab##classList##add (Js.string "active");
  nav_val.dom_content##classList##add (Js.string "in");
  nav_val.dom_content##classList##add (Js.string "active")

let navigation_value_set_unactive nav_val =
  nav_val.dom_tab##classList##remove (Js.string "active");
  nav_val.dom_content##classList##remove (Js.string "in");
  nav_val.dom_content##classList##remove (Js.string "active")

let navigation_element_find_active navigation_system =
  let active_value =
    NavigationElementHashtbl.fold begin fun _ nav_val acc ->
      if navigation_value_is_active nav_val then
        match acc with
        | Some _ ->
           raise (Web_exception Active_navigation_element_is_not_unique)
        | None ->
           Some nav_val
      else
        acc
    end navigation_system.navigation_elements None
  in
  match active_value with
  | Some x -> x
  | None -> raise (Web_exception No_active_navigation_element)

let navigation_element_create navigation_system ne tab_content_attach_creator =
  let nav_val =
    try
      NavigationElementHashtbl.find
        navigation_system.navigation_elements
        ne
    with
    | Not_found ->
       let tab, content, attach = tab_content_attach_creator () in
       let default_nav_val = {
         dom_tab = Tyxml_js.To_dom.of_element tab;
         dom_content = Tyxml_js.To_dom.of_element content;
         tab_is_open = false;
         content_is_created = false;
         attached_data = attach;
       }
       in
       NavigationElementHashtbl.add
         navigation_system.navigation_elements
         ne
         default_nav_val;
       default_nav_val
  in
  if not nav_val.tab_is_open then begin
    Dom.appendChild
      navigation_system.navigation_dom_tabs
      nav_val.dom_tab;
    nav_val.tab_is_open <- true;
    if not nav_val.content_is_created then begin
      Dom.appendChild
        navigation_system.navigation_dom_contents
        nav_val.dom_content;
      nav_val.content_is_created <- true;
    end
  end;
  nav_val

let navigation_element_close navigation_system ne =
  let nav_val =
    NavigationElementHashtbl.find
      navigation_system.navigation_elements
      ne
  in
  let default =
    NavigationElementHashtbl.find
      navigation_system.navigation_elements
      HomeElement
  in
  if navigation_value_is_active nav_val then begin
    navigation_value_set_active default
  end;
  nav_val.tab_is_open <- false;
  navigation_value_set_unactive nav_val;
  Dom.removeChild
    navigation_system.navigation_dom_tabs
    nav_val.dom_tab

let navigation_element_tab_id = function
  | HomeElement ->
     "home-tab"
  | WarningsElement ->
     "warnings-tab"
  | ErrorsElement ->
     "errors-tab"
  | FileElement file_info ->
     "file-" ^ (Digest.to_hex file_info.file_hash) ^ "-tab"

let navigation_element_content_id = function
  | HomeElement ->
     "home-content"
  | WarningsElement ->
     "warnings-content"
  | ErrorsElement ->
     "errors-content"
  | FileElement file_info ->
     "file-" ^ (Digest.to_hex file_info.file_hash) ^ "-content"

let model_simple_tab name ne =
  li
    ~a:[
      a_id (navigation_element_tab_id ne);
    ]
    [
      a
        ~a:[
          a_href ("#" ^ (navigation_element_content_id ne));
          a_user_data "toggle" "tab";
        ]
        [pcdata name]
    ]

let model_closable_tab navigation_system name ne =
  let close_button =
    button
      ~a:[
        a_button_type `Button;
        a_class ["close"; "closeTab"];
      ]
      [pcdata "×"]
  in
  let tab =
    li
       ~a:[
         a_id (navigation_element_tab_id ne);
         a_class ["closableTab"];
      ]
      [
        a
          ~a:[
            a_href ("#" ^ (navigation_element_content_id ne));
            a_user_data "toggle" "tab";
          ]
          [
            close_button;
            span [pcdata name];
          ]
      ]
  in
  (Tyxml_js.To_dom.of_element close_button)##onclick <-Dom_html.handler begin
    fun evt ->
      Dom.preventDefault evt;
      Dom_html.stopPropagation evt;
      navigation_element_close navigation_system ne;
      Js._true
  end;
  tab

let model_simple_content data ne =
  div
    ~a:[
      a_id (navigation_element_content_id ne);
      a_class ["tab-pane"; "fade"];
    ]
    [data]

let create content_creator =
  let tabs =
    ul
      ~a:[
        a_class ["nav"; "nav-tabs"];
      ]
      []
  in
  let contents =
    div
      ~a:[
        a_class ["tab-content"];
      ]
      []
  in
  {
    navigation_elements = NavigationElementHashtbl.create 32;
    navigation_dom_tabs = Tyxml_js.To_dom.of_element tabs;
    navigation_dom_contents = Tyxml_js.To_dom.of_element contents;
    navigation_content_creator = content_creator;
  }

let tab_content_attach_creator navigation_system ne =
  let tab_creator =
    match ne with
    | HomeElement ->
       model_simple_tab "home"
    | WarningsElement ->
       model_simple_tab "warnings"
    | ErrorsElement ->
       model_simple_tab "errors"
    | FileElement file ->
       model_closable_tab navigation_system (Web_utils.file_short_name file)
  in
  let content, attach =
    navigation_system.navigation_content_creator navigation_system ne
  in
  tab_creator ne,
  model_simple_content content ne,
  attach

let add_navigation_element navigation_system ne =
  navigation_element_create
    navigation_system
    ne
    (fun () -> tab_content_attach_creator navigation_system ne)

let init navigation_system =
  let home_nav_val =
    (add_navigation_element navigation_system HomeElement)
  in
  navigation_value_set_active home_nav_val;
  ignore
    (add_navigation_element navigation_system WarningsElement);
  ignore
    (add_navigation_element navigation_system ErrorsElement)

let open_tab navigation_system navigation_element =
  let navigation_value =
    add_navigation_element navigation_system navigation_element
  in
  navigation_value_set_unactive
    (navigation_element_find_active navigation_system);
  navigation_value_set_active navigation_value;
  navigation_value.attached_data

let open_home_tab navigation_system =
  match open_tab navigation_system HomeElement with
  | No_attached_data -> ()
  | _ -> raise (Web_exception (Invalid_content_attached_data "home"))

let open_warnings_tab navigation_system =
  match open_tab navigation_system WarningsElement with
  | No_attached_data -> ()
  | _ -> raise (Web_exception (Invalid_content_attached_data "warnings"))

let open_errors_tab navigation_system =
  match open_tab navigation_system ErrorsElement with
  | No_attached_data -> ()
  | _ -> raise (Web_exception (Invalid_content_attached_data "errors"))

let open_file_tab navigation_system file_info =
  match open_tab navigation_system (FileElement file_info) with
  | File_content_attached_data file_content_data -> file_content_data
  | _ -> raise (Web_exception (Invalid_content_attached_data "file"))
