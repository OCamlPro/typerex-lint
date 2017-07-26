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
  | File_content_attached_data of Web_file_content_data.file_content_data

type navigation_value = {
  dom_tab : Dom_html.element Js.t;
  dom_content : Dom_html.element Js.t;
  mutable tab_is_created : bool;
  mutable content_is_created : bool;
  attached_data : navigation_attached_data
}

module NavigationElement =
  struct
    type t = navigation_element
    let equal x y =
      match x,y with
      | HomeElement, HomeElement -> true
      | WarningsElement, WarningsElement -> true
      | ErrorsElement, ErrorsElement -> true
      | FileElement f, FileElement f' -> Web_utils.file_equals f f'
      | _ -> false
    let hash = Hashtbl.hash
  end

module NavigationElementHashtbl = Hashtbl.Make(NavigationElement)

type navigation_informations = {
  navigation_elements : navigation_value NavigationElementHashtbl.t;
  navigation_dom_tabs : Dom_html.element Js.t;
  navigation_dom_contents : Dom_html.element Js.t;
}

let global_navigation_informations =
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

let navigation_element_find_active () =
  let active_value =
    NavigationElementHashtbl.fold begin fun _ nav_val acc ->
    if navigation_value_is_active nav_val then
      match acc with
      | Some _ -> raise (Web_exception ActiveNavigationElementIsNotUnique)
      | None -> Some nav_val
    else
      acc
    end global_navigation_informations.navigation_elements None
  in
  match active_value with
  | Some x -> x
  | None -> raise (Web_exception NoActiveNavigationElement)

let navigation_element_static_create ne tab content attached =
  let tab = Tyxml_js.To_dom.of_element tab in
  let content = Tyxml_js.To_dom.of_element content in
  NavigationElementHashtbl.add
    global_navigation_informations.navigation_elements
    ne
    {
      dom_tab = tab;
      dom_content = content;
      tab_is_created = true;
      content_is_created = true;
      attached_data = attached;
    };
  Dom.appendChild global_navigation_informations.navigation_dom_tabs tab;
  Dom.appendChild global_navigation_informations.navigation_dom_contents content

(* return the attached data *)
let navigation_element_dynamic_create
      ne tab_creator content_creator attached_creator =
  let nav_val =
    try
      NavigationElementHashtbl.find
        global_navigation_informations.navigation_elements
        ne
    with
    | Not_found ->
       let default_nav_val = {
         dom_tab = Tyxml_js.To_dom.of_element (tab_creator ());
         dom_content = Tyxml_js.To_dom.of_element (content_creator ());
         tab_is_created = false;
         content_is_created = false;
         attached_data = attached_creator ();
       }
       in
       NavigationElementHashtbl.add
         global_navigation_informations.navigation_elements
         ne
         default_nav_val;
       default_nav_val
  in
  if not nav_val.tab_is_created then begin
    Dom.appendChild
      global_navigation_informations.navigation_dom_tabs
      nav_val.dom_tab;
    nav_val.tab_is_created <- true;
    if not nav_val.content_is_created then begin
      Dom.appendChild
        global_navigation_informations.navigation_dom_contents
        nav_val.dom_content
      ;
      nav_val.content_is_created <- true;
    end
  end;
  (* focus *)
  navigation_value_set_unactive (navigation_element_find_active ());
  navigation_value_set_active nav_val;
  nav_val.attached_data

let navigation_element_close ne =
  let nav_val =
    NavigationElementHashtbl.find
      global_navigation_informations.navigation_elements
      ne
  in
  let default =
    NavigationElementHashtbl.find
      global_navigation_informations.navigation_elements
      HomeElement
  in
  if navigation_value_is_active nav_val then begin
    navigation_value_set_active default
  end;
  nav_val.tab_is_created <- false;
  navigation_value_set_unactive nav_val;
  Dom.removeChild
    global_navigation_informations.navigation_dom_tabs
    nav_val.dom_tab

let navigation_element_set_active ne =
  let nav_val =
    NavigationElementHashtbl.find
      global_navigation_informations.navigation_elements
      ne
  in
  navigation_value_set_active nav_val

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

let model_closable_tab name ne =
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
      navigation_element_close ne;
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

let create_static_element ne tab_creator content_creator attached =
  navigation_element_static_create
    ne
    (tab_creator ne)
    (content_creator ne)
    attached

let create home_content warnings_content errors_content =
  create_static_element
    HomeElement
    (model_simple_tab "home")
    (model_simple_content home_content)
    No_attached_data;
  navigation_element_set_active HomeElement;
  create_static_element
    WarningsElement
    (model_simple_tab "warnings")
    (model_simple_content warnings_content)
    No_attached_data;
  create_static_element
    ErrorsElement
    (model_simple_tab "errors")
    (model_simple_content errors_content)
    No_attached_data;
  Tyxml_js.Of_dom.of_element
    global_navigation_informations.navigation_dom_tabs,
  Tyxml_js.Of_dom.of_element
    global_navigation_informations.navigation_dom_contents

let create_dynamic_element ne tab_creator content_creator attached_creator =
  navigation_element_dynamic_create
    ne
    (fun () -> tab_creator ne)
    (fun () -> content_creator ne)
    attached_creator

let open_tab navigation_element tab_name content attach_generator =
  (* todo improve *)
  (* navigation_element_dynamic_create *)
  (*   navigation_element *)
  (*   (fun () -> model_closable_tab tab_name) *)
  (*   (fun () -> model_simple_content content) *)
  (*   attach_generator *)
  create_dynamic_element
    navigation_element
    (model_closable_tab tab_name)
    (model_simple_content content)
    attach_generator
