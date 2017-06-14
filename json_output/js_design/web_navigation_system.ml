open Tyxml_js.Html
open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json

type dynamic_navigation_element =
    database_warning_entry

let compare_dynamic_navigation_element x y =
  (****) Pervasives.compare x.id y.id (****)
      
module DynamicNavigationElement =
  struct
    type t = dynamic_navigation_element
    let compare = compare_dynamic_navigation_element
  end
       
module DynamicNavigationElementSet = Set.Make(DynamicNavigationElement)    

let opened_dynamic_tab = ref DynamicNavigationElementSet.empty

let dynamic_navigation_tab_is_open dne =
  DynamicNavigationElementSet.exists begin fun open_dne ->
    compare_dynamic_navigation_element open_dne dne = 0
  end !opened_dynamic_tab
		       
let dynamic_navigation_tab_open dne =
  opened_dynamic_tab :=
    DynamicNavigationElementSet.add dne !opened_dynamic_tab

let dynamic_navigation_tab_remove dne =
  opened_dynamic_tab :=
    DynamicNavigationElementSet.remove dne !opened_dynamic_tab
		       
let navigation_is_active_tab tab =
  Js.to_bool (tab##classList##contains (Js.string "active"))

let navigation_set_active_tab tab =
  tab##classList##add (Js.string "active")

let navigation_set_unactive_tab tab =
  tab##classList##remove (Js.string "active")

let navigation_set_active_content content =
  content##classList##add (Js.string "in");
  content##classList##add (Js.string "active")

let navigation_set_unactive_content content =
  content##classList##remove (Js.string "in");
  content##classList##remove (Js.string "active")

let model_simple_tab name content_id =
  let tab = 
    li
      [
	a
          ~a:[
	    a_href ("#" ^ content_id);
	  a_user_data "toggle" "tab";
	  ]
	  [pcdata name]
      ]
  in
  Tyxml_js.To_dom.of_element tab

let model_closable_tab name content_id on_click =
  let close_button =
    button
      ~a:[
	a_button_type `Button;
	a_class ["close"; "closeTab"];	      
      ]
      [pcdata "×"]
      
  in
  let a_tab =
    a
      ~a:[
	a_href ("#" ^ content_id);
	a_user_data "toggle" "tab";
      ]
      [close_button; pcdata name]
  in
  let tab = 
    li
      [
	a_tab
      ]
  in
  let dom_tab = Tyxml_js.To_dom.of_element tab in
  (Tyxml_js.To_dom.of_element a_tab)##onclick <-Dom_html.handler begin
    fun evt ->
    Dom.preventDefault evt;
      Js._true
  end;
  (Tyxml_js.To_dom.of_element close_button)##onclick <-Dom_html.handler begin
    fun evt ->
    Dom_html.stopPropagation evt;							   
      on_click dom_tab;
      Js._true
  end;
  dom_tab

let model_simple_content data content_id =
  let content =
    div
      ~a:[
	a_id content_id;
	a_class ["tab-pane"; "fade"];
      ]
      [data]
  in
  Tyxml_js.To_dom.of_element content
	 
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
  
let home_content_id =
  "home-content"
    
let navigation_home_tab () =
  model_simple_tab
    "home"
    home_content_id

let navigation_home_content home_content =
  let content =
    model_simple_content
      home_content
      home_content_id
  in
  navigation_set_active_content content;
  content
    
let linter_content_id =
  "linters-content"
  
let navigation_linter_tab () =
  model_simple_tab
    "linters"
    linter_content_id
    
let navigation_linter_content linter_content =
  model_simple_content
    linter_content
    linter_content_id
    
let warning_content_id warning_entry =
  "warning-" ^ (string_of_int warning_entry.id) ^ "-content"

let navigation_warning_tab warning_entry =
  let close_tab tab =
    if navigation_is_active_tab tab then begin
      navigation_set_active_tab tab
    end;
    dynamic_navigation_tab_remove warning_entry;
    Dom.removeChild dom_tabs tab
  in 
  model_closable_tab
    (string_of_int warning_entry.id)
    (warning_content_id warning_entry)
    close_tab
						    
let navigation_warning_content warning_entry warning_content =
  model_simple_content
    warning_content
    (warning_content_id warning_entry)
    
let init home_content linter_content warnings_and_contents =
  let wcontents =
    List.map
      (fun (we, wc) -> navigation_warning_content we wc)
      warnings_and_contents
  in
  let nav_home_tab = navigation_home_tab () in
  let nav_home_content = navigation_home_content home_content in
  navigation_set_active_tab nav_home_tab;
  navigation_set_active_content nav_home_content;
  Dom.appendChild dom_tabs nav_home_tab;
  Dom.appendChild dom_tabs (navigation_linter_tab ());
  Dom.appendChild dom_contents nav_home_content;
  Dom.appendChild dom_contents (navigation_linter_content linter_content);
  (* todo remove in lazy gen *)
  List.iter begin fun wcontent ->
		  Dom.appendChild dom_contents wcontent
	    end wcontents
(* *)
    
let navigation_open_warning_tab_content warning =
  if not (dynamic_navigation_tab_is_open warning) then begin
    let tab = navigation_warning_tab warning in
    Dom.appendChild dom_tabs tab;
    dynamic_navigation_tab_open warning
  end

