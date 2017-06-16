open Tyxml_js.Html
open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json

type dynamic_navigation_element =
    database_warning_entry

type dynamic_navigation_value = {
  tab_is_open : bool;
  content_is_created : bool;
}
      
module DynamicNavigationElement =
  struct
    type t = dynamic_navigation_element
    let equal = (* *) (=) (* *)
    let hash e = 0
  end

let default_dynamic_navigation_value =
  {tab_is_open = false; content_is_created = false;}
    
module DynamicNavigationElementHashtbl = Hashtbl.Make(DynamicNavigationElement)

let opened_dynamic_elements = DynamicNavigationElementHashtbl.create 32

let dynamic_navigation_get_value dne =
  try 
    (DynamicNavigationElementHashtbl.find opened_dynamic_elements dne)
  with
  | Not_found -> default_dynamic_navigation_value
								     
let dynamic_navigation_tab_is_open dne =
  let value = dynamic_navigation_get_value dne in
  value.tab_is_open

let dynamic_navigation_content_is_created dne =
  let value = dynamic_navigation_get_value dne in
  value.content_is_created
		       
let dynamic_navigation_tab_open dne =
  let value = dynamic_navigation_get_value dne in
  DynamicNavigationElementHashtbl.add
    opened_dynamic_elements
    dne
    {value with tab_is_open = true;}

let dynamic_navigation_content_create dne =
  let value = dynamic_navigation_get_value dne in
  DynamicNavigationElementHashtbl.add
    opened_dynamic_elements
    dne
    {value with content_is_created = true;}

let dynamic_navigation_tab_close dne =
  let value = dynamic_navigation_get_value dne in
  DynamicNavigationElementHashtbl.add
    opened_dynamic_elements
    dne
    {value with tab_is_open = false;}
		       
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
	 
let model_simple_tab name tab_id content_id =
  let tab = 
    li
      ~a:[
	a_id tab_id;
      ]
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

let model_closable_tab name tab_id content_id on_click =
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
       ~a:[
	a_id tab_id;
      ]
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

let get_tab_list () =
  dom_tabs##childNodes
  |> Dom.list_of_nodeList
  |> List.map begin fun child ->
       Js.Opt.get
	 (Dom_html.CoerceTo.element child)
	 (fun () -> failwith "coerce error")
     end

let get_content_list () =
  dom_contents##childNodes
  |> Dom.list_of_nodeList
  |> List.map begin fun child ->
       Js.Opt.get
	 (Dom_html.CoerceTo.element child)
	 (fun () -> failwith "coerce error")
     end

let default_tab : Dom_html.element Js.t option ref = ref None

let get_default_tab () =
  match !default_tab with
  | Some tab -> tab
  | None -> failwith "navigation system is not init : no default tab"

let default_content : Dom_html.element Js.t option ref = ref None

let get_default_content () =
  match !default_content with
  | Some content -> content
  | None -> failwith "navigation system is not init : no default content"

let warning_content_creator_ref :
      (Lint_warning_json.database_warning_entry ->
       Dom_html.element Js.t) option ref =
  ref None
		     
let get_warning_content_creator () =
  match !warning_content_creator_ref with
  | Some content -> content
  | None -> failwith "navigation system is not init : no warning content creator"
  
let home_tab_id =
  "home-tab"
	      
let home_content_id =
  "home-content"

let linter_tab_id =
  "linters-tab"
    
let linter_content_id =
  "linters-content"

let warning_tab_id warning_entry =
  "warning-" ^ (string_of_int warning_entry.id) ^ "-tab"
    
let warning_content_id warning_entry =
  "warning-" ^ (string_of_int warning_entry.id) ^ "-content"

let navigation_home_content home_content =
  let content =
    model_simple_content
      home_content
      home_content_id
  in
  navigation_set_active_content content;
  content
						    
let navigation_home_tab () =
  model_simple_tab
    "home"
    home_tab_id
    home_content_id

let navigation_linter_content linter_content =
  model_simple_content
    linter_content
    linter_content_id
    
let navigation_linter_tab () =
  model_simple_tab
    "linters"
    linter_tab_id
    linter_content_id

let navigation_warning_content warning_entry warning_content =
  model_simple_content
    warning_content
    (warning_content_id warning_entry)

let navigation_warning_tab warning_entry =
  let close_tab tab =
    if navigation_is_active_tab tab then begin
      let content_id = Js.string (warning_content_id warning_entry) in
      let content =
	List.find (fun content -> content##id = content_id) (get_content_list ())
      in
      navigation_set_active_tab (get_default_tab ());
      navigation_set_active_content (get_default_content ());
      navigation_set_unactive_content content
    end;
    dynamic_navigation_tab_close warning_entry;
    Dom.removeChild dom_tabs tab
  in
  let tab =
    model_closable_tab
      (string_of_int warning_entry.id)
      (warning_tab_id warning_entry)
      (warning_content_id warning_entry)
      close_tab
  in
  tab
    
let init home_content linter_content warning_content_creator =
  let nav_home_tab = navigation_home_tab () in
  let nav_home_content = navigation_home_content home_content in
  navigation_set_active_tab nav_home_tab;
  navigation_set_active_content nav_home_content;
  default_tab := Some nav_home_tab;
  default_content := Some nav_home_content;
  warning_content_creator_ref := Some warning_content_creator;
  Dom.appendChild dom_tabs nav_home_tab;
  Dom.appendChild dom_tabs (navigation_linter_tab ());
  Dom.appendChild dom_contents nav_home_content;
  Dom.appendChild dom_contents (navigation_linter_content linter_content)
	    
let navigation_open_warning_tab_content warning =
  if not (dynamic_navigation_tab_is_open warning) then begin
    let tab = navigation_warning_tab warning in
    Dom.appendChild dom_tabs tab;
    dynamic_navigation_tab_open warning
  end;
  if not (dynamic_navigation_content_is_created warning) then begin
    let creator = get_warning_content_creator () in
    let warning_content =
      navigation_warning_content
	warning
	(Tyxml_js.Of_dom.of_element (creator warning))
    in
    Dom.appendChild dom_contents warning_content;
    dynamic_navigation_content_create warning
  end
							 

