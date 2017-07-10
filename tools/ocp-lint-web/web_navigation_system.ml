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
  
type navigation_element =
  | HomeElement
  | PluginsElement
  | LinterElement
  | ResultElement
  | WarningElement of warning_info

type navigation_value = {
  dom_tab : Dom_html.element Js.t;
  dom_content : Dom_html.element Js.t;
  mutable tab_is_created : bool;
  mutable content_is_created : bool;
}

module NavigationElement =
  struct
    type t = navigation_element
    let equal x y =
      match x,y with
      | HomeElement, HomeElement -> true
      | LinterElement, LinterElement -> true
      | ResultElement, ResultElement -> true
      | WarningElement x, WarningElement y -> x = y
      | _ -> false
    let hash e = 0 (* todo *)
  end
 
module NavigationElementHashtbl = Hashtbl.Make(NavigationElement)

let navigation_elements = NavigationElementHashtbl.create 32

let tabs =
  ul
    ~a:[
      a_class ["nav"; "nav-tabs"];
    ]
    []

let dom_tabs =
  Tyxml_js.To_dom.of_element tabs

let contents =
  div
    ~a:[
      a_class ["tab-content"];
    ]
    []

let dom_contents = 
  Tyxml_js.To_dom.of_element contents

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

let navigation_element_static_create ne tab content =
  let tab = Tyxml_js.To_dom.of_element tab in
  let content = Tyxml_js.To_dom.of_element content in
  NavigationElementHashtbl.add navigation_elements ne {
    dom_tab = tab;
    dom_content = content;
    tab_is_created = true;
    content_is_created = true;
  };
  Dom.appendChild dom_tabs tab;
  Dom.appendChild dom_contents content

let navigation_element_dynamic_create ne tab_creator content_creator =
  let nav_val =
    try
      NavigationElementHashtbl.find navigation_elements ne
    with
    | Not_found ->
       let default_nav_val = {
         dom_tab = Tyxml_js.To_dom.of_element (tab_creator ());
         dom_content = Tyxml_js.To_dom.of_element (content_creator ());
         tab_is_created = false;
	 content_is_created = false;
       }
       in
       NavigationElementHashtbl.add navigation_elements ne default_nav_val;
       default_nav_val
  in
  if not nav_val.tab_is_created then begin
    Dom.appendChild dom_tabs nav_val.dom_tab;
    nav_val.tab_is_created <- true;
    if not nav_val.content_is_created then begin
      Dom.appendChild dom_contents nav_val.dom_content;
      nav_val.content_is_created <- true;
    end
  end

let navigation_element_close ne =
  let nav_val = NavigationElementHashtbl.find navigation_elements ne in
  let default = NavigationElementHashtbl.find navigation_elements HomeElement in
  if navigation_value_is_active nav_val then begin
    navigation_value_set_active default
  end;
  nav_val.tab_is_created <- false;
  navigation_value_set_unactive nav_val;
  Dom.removeChild dom_tabs nav_val.dom_tab

let navigation_element_set_active ne =
  let nav_val = NavigationElementHashtbl.find navigation_elements ne in
  navigation_value_set_active nav_val

let navigation_element_tab_id = function
  | HomeElement ->
     "home-tab"
  | PluginsElement ->
     "plugins-tab"
  | LinterElement ->
     "linters-tab"
  | ResultElement ->
     "results-tab"
  | WarningElement warning_info ->
     "warning-" ^ (string_of_int warning_info.warning_id) ^ "-tab"

let navigation_element_content_id = function
  | HomeElement ->
     "home-content"
  | PluginsElement ->
     "plugins-content"
  | LinterElement ->
     "linters-content"
  | ResultElement ->
     "results-content"
  | WarningElement warning_info ->
     "warning-" ^ (string_of_int warning_info.warning_id) ^ "-content"

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

let create_static_element ne tab_creator content_creator =
  navigation_element_static_create ne (tab_creator ne) (content_creator ne)
    
let create home_content plugins_content linter_content result_content =
  create_static_element
    HomeElement
    (model_simple_tab "home")
    (model_simple_content home_content);
  navigation_element_set_active HomeElement;
  create_static_element
    PluginsElement
    (model_simple_tab "plugins")
    (model_simple_content plugins_content);
  create_static_element
    LinterElement
    (model_simple_tab "linters")
    (model_simple_content linter_content);
  create_static_element
    ResultElement
    (model_simple_tab "results")
    (model_simple_content result_content);
  tabs, contents

let create_dynamic_element ne tab_creator content_creator =
  navigation_element_dynamic_create
    ne
    (fun () -> tab_creator ne)
    (fun () -> content_creator ne)

let open_warning_tab warning_info warning_content =
  let warning_element = WarningElement warning_info in
  create_dynamic_element
    warning_element
    (model_closable_tab (string_of_int warning_info.warning_id))
    (model_simple_content warning_content)
