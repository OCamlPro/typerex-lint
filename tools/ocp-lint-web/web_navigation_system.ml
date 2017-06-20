open Tyxml_js.Html
open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json

type navigation_element =
  | HomeElement
  | LinterElement
  | WarningElement of database_warning_entry

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
      | WarningElement x, WarningElement y -> x = y
      | _ -> false
    let hash e = 0
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
  | LinterElement ->
     "linters-tab"
  | WarningElement warning_entry ->
     "warning-" ^ (string_of_int warning_entry.id) ^ "-tab"

let navigation_element_content_id = function
  | HomeElement ->
     "home-content"
  | LinterElement ->
     "linters-content"
  | WarningElement warning_entry ->
     "warning-" ^ (string_of_int warning_entry.id) ^ "-content"
						    
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
    
let create home_content linter_content =
  navigation_element_static_create
    HomeElement
    (model_simple_tab "home" HomeElement)
    (model_simple_content home_content HomeElement);
  navigation_element_set_active HomeElement; 
  navigation_element_static_create
    LinterElement
    (model_simple_tab "linters" LinterElement)
    (model_simple_content linter_content LinterElement);
  tabs, contents

let navigation_open_warning_tab_content warning warning_content =
  (* todo rename *)
  let warning_element = WarningElement warning in
  let tab_creator () =
    model_closable_tab (string_of_int warning.id) warning_element
  in
  let content_creator () =
    model_simple_content warning_content warning_element
  in
  navigation_element_dynamic_create
    (WarningElement warning)
    tab_creator
    content_creator
