open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json

(*******)
let find_component id =
  match Js_utils.Manip.by_id id with
  | Some div -> div
  | None -> failwith ("Cannot find id " ^ id)
(*******)
       
let doc = Dom_html.document

let close_button () =
  let button = Dom_html.createButton doc in
  let span = Dom_html.createSpan doc in
  button##setAttribute(Js.string "type", Js.string "buton");
  button##setAttribute(Js.string "class", Js.string "close");
  button##setAttribute(Js.string "aria-label", Js.string "Close");
  span##setAttribute(Js.string "aria-hidden", Js.string "true");
  span##innerHTML <- Js.string "&times;";
  Dom.appendChild button span;
  button

let navigation_is_active_tab tab =
  Js.to_bool (tab##classList##contains (Js.string "active"))
	    
let navigation_plugin_tab_menu plugins =
  let ul = Dom_html.createUl doc in
  ul##setAttribute(Js.string "class", Js.string "dropdown-menu");
  List.iter begin fun plugin ->
    let li = Dom_html.createLi doc in
    let a = Dom_html.createA doc in
    a##setAttribute(Js.string "href", Js.string "#");
    a##innerHTML <- Js.string plugin;
    Dom.appendChild li a;
    Dom.appendChild ul li
  end plugins;
  ul

let navigation_home_content_id =
  "home-content"

let navigation_home_tab_id =
  "home-tab"
    
let navigation_home_tab =
  let li = Dom_html.createLi doc in
  let a = Dom_html.createA doc in
  let href = "#" ^ (navigation_home_content_id) in
  li##setAttribute(Js.string "id", Js.string navigation_home_tab_id);
  (* todo set_active_tab *)
  li##setAttribute(Js.string "class", Js.string "active");
  a##setAttribute(Js.string "href", Js.string href);
  a##setAttribute(Js.string "data-toggle", Js.string "tab");
  a##innerHTML <- Js.string "home";
  Dom.appendChild li a;
  li
	    
let navigation_plugin_tab plugins =
  let li = Dom_html.createLi doc in
  let span = Dom_html.createSpan doc in
  let a = Dom_html.createA doc in
  span##setAttribute(Js.string "class", Js.string "caret dropdown");
  li##setAttribute(Js.string "class", Js.string "dropdown");
  a##setAttribute(Js.string "class", Js.string "dropdown-toggle");
  a##setAttribute(Js.string "data-toggle", Js.string "dropdown");
  a##setAttribute(Js.string "href", Js.string "#");
  a##innerHTML <- Js.string "plugins";
  Dom.appendChild a span;
  Dom.appendChild li a;
  Dom.appendChild li (navigation_plugin_tab_menu plugins);
  li
  
let navigation_linter_tab =
  let li = Dom_html.createLi doc in
  let a = Dom_html.createA doc in
  a##setAttribute(Js.string "href", Js.string "#");
  a##setAttribute(Js.string "data-toggle", Js.string "tab");
  a##innerHTML <- Js.string "linters";
  Dom.appendChild li a;
  li

let navigation_warning_content_id entry =
  "warning-" ^ (string_of_int entry.id) ^ "-content"

let navigation_warning_tab_id entry =
  "warning-" ^ (string_of_int entry.id) ^ "-tab"
    
let navigation_warning_tab on_close entry =
  let li = Dom_html.createLi doc in
  let a = Dom_html.createA doc in
  let button = close_button () in
  let href = "#" ^ (navigation_warning_content_id entry) in
  li##setAttribute(Js.string "id", Js.string (navigation_warning_tab_id entry));
  a##setAttribute(Js.string "href", Js.string href);
  a##setAttribute(Js.string "data-toggle", Js.string "tab");
  a##innerHTML <- Js.string (string_of_int entry.id);
  button##onclick <- Dom_html.handler begin fun _ ->
    on_close li;
    Js._true
  end;
  Dom.appendChild li a;
  Dom.appendChild li button;
  li
    
let navigation_tabs entries  =
  let plugins =
    List.map fst
      (group_by ~clss:(fun entry -> entry.plugin_name) ~lst:entries)
  in
  let ul = Dom_html.createUl doc in
  ul##setAttribute(Js.string "class", Js.string "nav nav-tabs");
  List.iter (fun tab ->  Dom.appendChild ul tab) [
    navigation_home_tab;
    navigation_plugin_tab plugins;
    navigation_linter_tab
  ];
  ul
