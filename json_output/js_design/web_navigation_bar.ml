open Tyxml_js.Html
open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json

type dynamic_tab =
    database_warning_entry

let compare_dynamic_tab x y =
  (****) Pervasives.compare x.id y.id (****)
      
module DynamicTab =
  struct
    type t = dynamic_tab
    let compare = compare_dynamic_tab
  end
       
module DynamicTabSet = Set.Make(DynamicTab)    

let opened_dynamic_tab = ref DynamicTabSet.empty

let dynamic_tab_is_open dt =
  DynamicTabSet.exists begin fun open_dt ->
    compare_dynamic_tab open_dt dt = 0
  end !opened_dynamic_tab
		       
let dynamic_tab_open dt =
  opened_dynamic_tab := DynamicTabSet.add dt !opened_dynamic_tab

let dynamic_tab_remove dt =
  opened_dynamic_tab := DynamicTabSet.remove dt !opened_dynamic_tab
		       
let navigation_is_active_tab tab =
  Js.to_bool (tab##classList##contains (Js.string "active"))
		     
let close_button () =
  button
    ~a:[
      a_button_type `Button;
      a_class ["close"];
      (* "aria-label" "Close" *)	      
    ]
    [
      span
	~a:[
	  (* "aria-hidden" "true" *)
	]
	[
	  pcdata "×"
	]
    ]

let navigation_simple_tab name tab_id content_id =
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

let navigation_closable_tab name tab_id content_id =
  let close_button =
    button
      ~a:[
	a_button_type `Button;
	a_class ["close"];
	(* "aria-label" "Close" *)	      
      ]
      [
	span
	  ~a:[
	    (* "aria-hidden" "true" *)
	  ]
	  [
	    pcdata "×"
	  ]
      ]
  in
  li
    ~a:[
      a_id tab_id;
    ]
    [
      close_button;
      a
        ~a:[
	  a_href ("#" ^ content_id);
	  a_user_data "toggle" "tab";
	]
	[pcdata name]
    ]
	    
(* let navigation_plugin_tab_menu plugins = *)
(*   let ul = Dom_html.createUl doc in *)
(*   ul##setAttribute(Js.string "class", Js.string "dropdown-menu"); *)
(*   List.iter begin fun plugin -> *)
(*     let li = Dom_html.createLi doc in *)
(*     let a = Dom_html.createA doc in *)
(*     a##setAttribute(Js.string "href", Js.string "#"); *)
(*     a##innerHTML <- Js.string plugin; *)
(*     Dom.appendChild li a; *)
(*     Dom.appendChild ul li *)
(*   end plugins; *)
(*   ul *)

(* let navigation_plugin_tab plugins = *)
(*   let li = Dom_html.createLi doc in *)
(*   let span = Dom_html.createSpan doc in *)
(*   let a = Dom_html.createA doc in *)
(*   span##setAttribute(Js.string "class", Js.string "caret dropdown"); *)
(*   li##setAttribute(Js.string "class", Js.string "dropdown"); *)
(*   a##setAttribute(Js.string "class", Js.string "dropdown-toggle"); *)
(*   a##setAttribute(Js.string "data-toggle", Js.string "dropdown"); *)
(*   a##setAttribute(Js.string "href", Js.string "#"); *)
(*   a##innerHTML <- Js.string "plugins"; *)
(*   Dom.appendChild a span; *)
(*   Dom.appendChild li a; *)
(*   Dom.appendChild li (navigation_plugin_tab_menu plugins); *)
(*   li *)
    
let navigation_home_content_id =
  "home-content"

let navigation_home_tab_id =
  "home-tab"
    
let navigation_home_tab =
  let tab = 
    navigation_simple_tab
      "home"
      navigation_home_tab_id
      navigation_home_content_id
  in
  (* set active *)
  (Tyxml_js.To_dom.of_element tab)##classList##add (Js.string "active");
  (*            *)
  tab
    
let navigation_linter_content_id =
  "linters-content"
    
let navigation_linter_tab_id =
  "linters-tab"
  
let navigation_linter_tab =
 navigation_simple_tab
    "linters"
    navigation_linter_tab_id
    navigation_linter_content_id

let navigation_warning_content_id warning_entry =
  "warning-" ^ (string_of_int warning_entry.id) ^ "-content"

let navigation_warning_tab_id warning_entry =
  "warning-" ^ (string_of_int warning_entry.id) ^ "-tab"
			     
let navigation_warning_tab on_close warning_entry =
  (* let tab = *)
  (*   navigation_closable_tab *)
  (*     (string_of_int warning_entry.id) *)
  (*     (navigation_warning_tab_id warning_entry) *)
  (*     (navigation_warning_content_id warning_entry) *)
  (*     (\****\) on_close (\****\) *)
  (* in *)
  (* Tyxml_js.To_dom.of_element tab *)
  
  let tab =
    navigation_simple_tab
      (string_of_int warning_entry.id)
      (navigation_warning_tab_id warning_entry)
      (navigation_warning_content_id warning_entry)
  in
  let dom_tab = Tyxml_js.To_dom.of_element tab in
  let dom_button = Tyxml_js.To_dom.of_element (close_button ()) in
  dom_button##onclick <- Dom_html.handler begin fun _ ->
    on_close dom_tab;
    Js._true
  end;
  Dom.appendChild dom_tab dom_button;
  dom_tab

let navigation_open_warning_tab tabs warning =
  if dynamic_tab_is_open warning then begin
    let tab = navigation_warning_tab (* *) (fun _ -> ()) (* *) warning in
    Dom.appendChild tabs tab;
    dynamic_tab_open warning
  end
    
let navigation_tabs warning_entries  =
  (* let plugins = *)
  (*   List.map fst *)
  (*     (group_by ~clss:(fun entry -> entry.plugin_name) ~lst:warning_entries) *)
  (* in *)
  let tabs =
    ul
      ~a:[
	a_class ["nav"; "nav-tabs"];
      ]
      [
	navigation_home_tab;
	navigation_linter_tab;
      ]
  in
  Tyxml_js.To_dom.of_element tabs
  (* let ul = Dom_html.createUl doc in *)
  (* ul##setAttribute(Js.string "class", Js.string "nav nav-tabs"); *)
  (* List.iter (fun tab ->  Dom.appendChild ul tab) [ *)
  (*   Tyxml_js.To_dom.of_element navigation_home_tab; *)
  (*  Tyxml_js.To_dom.of_element navigation_linter_tab *)
  (* ]; *)
  (* ul *)


let navigation_tabs_system =
  (* utiliser celle-ci qu lieu de navigation_tab *)
  let tabs =
    ul
      ~a:[
	a_class ["nav"; "nav-tabs"];
      ]
      [
	navigation_home_tab;
	navigation_linter_tab;
      ]
  in
  let contents =
    div
      ~a:[
      ]
      [
      ]
  in
  tabs, contents
