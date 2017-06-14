open Lint_warning_types
open Lint_warning_json (****)
open Lint_plugin_json
open Web_navigation_bar

(*******)
let find_component id =
  match Js_utils.Manip.by_id id with
  | Some div -> div
  | None -> failwith ("Cannot find id " ^ id)
(*******)
       
let doc = Dom_html.document
	    
let summary_file_view filepath =
  let div = Dom_html.createDiv doc in
  (*let button = Dom_html.createButton doc in
  let display = ref false in
  let iframe = Dom_html.createIframe doc in
  button##innerHTML <- Js.string "show";
  button##onclick <- Dom_html.handler begin fun _ ->
    display := not (!display);
    let style =
      if !display then
	"display: block"
      else
	"display: none"
    in
    iframe##setAttribute(Js.string "style", Js.string style);
    Js._false;
  end;
  iframe##setAttribute(Js.string "src", Js.string filepath);
  iframe##setAttribute(Js.string "width", Js.string "700");
  iframe##setAttribute(Js.string "height", Js.string "300");
  iframe##setAttribute(Js.string "style", Js.string "display: none");
  Dom.appendChild div button;
  Dom.appendChild div iframe;*)
  div
    
let summary_entry_file_name_col entry =
  let p = Dom_html.createP doc in
  let a = Dom_html.createA doc in
  let href =
    (Digest.to_hex entry.hash) ^ ".html#" ^ (string_of_int entry.id)
  in
  a##innerHTML <- Js.string entry.file_name;
  Dom.appendChild p a;
  (* if not entry.warning_result.loc.Location.loc_ghost then begin *)
    a##setAttribute(Js.string "href", Js.string href);
    a##setAttribute(Js.string "target", Js.string "_blank");
    Dom.appendChild p (Dom_html.createBr doc);
    Dom.appendChild p (summary_file_view href);
  (* end; *)
  p

let summary_entry_plugin_name_col entry =
  let p = Dom_html.createP doc in
  p##innerHTML <- Js.string entry.plugin_name;
  p

let summary_entry_linter_name_col entry =
  let p = Dom_html.createP doc in
  p##innerHTML <- Js.string entry.linter_name;
  p
    
let summary_entry_warning_output_col tabs (***) content (***) entry =
  (* deplacer systeme gestion onglet *)
  let get_tab_list () =
    tabs##childNodes
    |> Dom.list_of_nodeList
    |> List.map begin fun child ->
         Js.Opt.get
	   (Dom_html.CoerceTo.element child)
	   (fun () -> failwith "coerce error")
       end
  in
  let default_tab =
    List.find begin fun child ->
      Js.to_string child##id = navigation_home_tab_id
    end (get_tab_list ())
  in
  let get_default_content () =
    let tmp =
      content##childNodes
      |> Dom.list_of_nodeList
      |> List.map begin fun child ->
         Js.Opt.get
	   (Dom_html.CoerceTo.element child)
	   (fun () -> failwith "coerce error")
		  end
    in
    List.find begin fun child ->
      Js.to_string child##id = navigation_home_content_id
    end tmp
  in
  let get_entry_content () =
    let tmp =
      content##childNodes
      |> Dom.list_of_nodeList
      |> List.map begin fun child ->
         Js.Opt.get
	   (Dom_html.CoerceTo.element child)
	   (fun () -> failwith "coerce error")
		  end
    in
    List.find begin fun child ->
      Js.to_string child##id = navigation_warning_content_id entry
    end tmp
  in
  let close_tab tab =
    if navigation_is_active_tab tab then begin
      (* set active *)
      default_tab##classList##add (Js.string "active")
      (*****)
				      ; let tmp = get_default_content () in
					let tmpp = get_entry_content () in

      tmpp##classList##remove (Js.string "in");
      tmpp##classList##remove (Js.string "active");
      tmp##classList##add (Js.string "active");
      tmp##classList##add (Js.string "in");
      (*****)
    end;
    Dom.removeChild tabs tab
  in
  let warning_id = navigation_warning_tab_id entry in
  let p = Dom_html.createP doc in
  let a = Dom_html.createA doc in
  a##innerHTML <- Js.string entry.warning_result.output;
  a##setAttribute(Js.string "href", Js.string "#");
  a##onclick <- Dom_html.handler begin fun _ ->
    let tab =
      try
	List.find begin fun child ->
          Js.to_string child##id = warning_id
	end (get_tab_list ())
      with	  
      | Not_found ->
	 let new_tab = navigation_warning_tab close_tab entry in
	 Dom.appendChild tabs new_tab;
	 new_tab
    in
    ignore tab; (* focus sur tab *);
    Js._true
  end;
  Dom.appendChild p a;
  p

let summary_warning_entry tabs content entry =
  let tr = Dom_html.createTr doc in
  let td_file_name = Dom_html.createTd doc in
  let td_plugin_name = Dom_html.createTd doc in
  let td_linter_name = Dom_html.createTd doc in
  let td_warning_output = Dom_html.createTd doc in
  Dom.appendChild td_file_name (summary_entry_file_name_col entry);
  Dom.appendChild tr td_file_name;
  Dom.appendChild td_plugin_name (summary_entry_plugin_name_col entry);
  Dom.appendChild tr td_plugin_name;
  Dom.appendChild td_linter_name (summary_entry_linter_name_col entry);
  Dom.appendChild tr td_linter_name;
  Dom.appendChild td_warning_output (summary_entry_warning_output_col tabs content entry);
  Dom.appendChild tr td_warning_output;
  tr

let summary_head_description =
  let thead = Dom_html.createThead doc in
  let tr = Dom_html.createTr doc in
  let th_file_name = Dom_html.createTh doc in
  let th_plugin_name = Dom_html.createTh doc in
  let th_linter_name = Dom_html.createTh doc in
  let th_warning_output = Dom_html.createTh doc in
  th_file_name##innerHTML <- Js.string "File";
  Dom.appendChild tr th_file_name;
  th_plugin_name##innerHTML <- Js.string "Plugin";
  Dom.appendChild tr th_plugin_name;
  th_linter_name##innerHTML <- Js.string "Linter";
  Dom.appendChild tr th_linter_name;
  th_warning_output##innerHTML <- Js.string "Warning";
  Dom.appendChild tr th_warning_output;
  Dom.appendChild thead tr;
  thead
  
let summary_table tabs content entries =
  let table = Dom_html.createTable doc in
  let tbody = Dom_html.createTbody doc in
  (*******)
  table##setAttribute(Js.string "id", Js.string "summary-table");
  table##setAttribute(Js.string "class", Js.string "display");
  table##setAttribute(Js.string "cellspacing", Js.string "0");
  table##setAttribute(Js.string "width", Js.string "100%");
  (*******)
  Dom.appendChild table (summary_head_description);
  List.iter begin fun entry ->
    Dom.appendChild tbody (summary_warning_entry tabs content entry);
  end entries;
  Dom.appendChild table tbody;
  table

let summary_page tabs content warnings_entries =
  let div = Dom_html.createDiv doc in
  div##setAttribute(Js.string "id", Js.string navigation_home_content_id);
  div##setAttribute(Js.string "class", Js.string "tab-pane fade in active");
  Dom.appendChild div (summary_table tabs content warnings_entries);
  div

let warning_description_header_code_view entry =
  let href =
    (Digest.to_hex entry.hash) ^ ".html#" ^ (string_of_int entry.id)
  in
  let div = Dom_html.createDiv doc in
  let a = Dom_html.createA doc in
  div##setAttribute(Js.string "class", Js.string "panel-heading");
  a##setAttribute(Js.string "href", Js.string href);
  a##innerHTML <- Js.string entry.file_name;
  Dom.appendChild div a;
  div

let warning_description_content_code_view entry =
  let href =
    (Digest.to_hex entry.hash) ^ ".html#" ^ (string_of_int entry.id)
  in
  let div = Dom_html.createDiv doc in
  let iframe = Dom_html.createIframe doc in

  div##setAttribute(Js.string "class", Js.string "panel-body");
  div##setAttribute(Js.string "style", Js.string "padding:0; height:170px");
  iframe##setAttribute(Js.string "src", Js.string href);
  iframe##setAttribute(Js.string "style", Js.string "width:100%; height:100%; border:none; border-radius:0 0 4px 4px;");
  
  (* (\***\) *)
  (* div##setAttribute(Js.string "style", Js.string "margin:0 5% 0 5%;"); *)
  (* iframe##setAttribute(Js.string "style", Js.string "width:100%;"); *)
  (* (\***\) *)
  (* (\* iframe##setAttribute(Js.string "width", Js.string "700"); *\) *)
  (* (\* iframe##setAttribute(Js.string "height", Js.string "600"); *\) *)
  (* Dom.appendChild div iframe; *)
  (* div *)
  (* (\* let div = Dom_html.createDiv doc in *\) *)
  (* (\* let descdiv = Dom_html.createDiv doc in *\) *)
  (* (\* let desc = entry.warning_result.output in *\) *)
  (* (\* descdiv##innerHTML <- Js.string desc; *\) *)
  (* (\* Dom.appendChild div descdiv; *\) *)
  (* (\* if not entry.warning_result.loc.Location.loc_ghost then begin *\) *)
  (* (\*   let a = Dom_html.createA doc in *\) *)
  (* (\*   let href = *\) *)
  (* (\*     (Digest.to_hex entry.hash) ^ ".html#" ^ (string_of_int entry.id) *\) *)
  (* (\*   in *\) *)
  (* (\*   a##innerHTML <- Js.string "voir le warning dans le fichier"; *\) *)
  (* (\*   a##setAttribute(Js.string "href", Js.string href); *\) *)
  (* (\*   a##setAttribute(Js.string "target", Js.string "_blank"); *\) *)
  (* (\*   Dom.appendChild div a; *\) *)
  (* (\* end; *\) *)

  Dom.appendChild div iframe; 
  div
    
let warning_description_code_view entry =
  let div = Dom_html.createDiv doc in
  div##setAttribute(Js.string "class", Js.string "panel panel-default");
  div##setAttribute(Js.string "style", Js.string "margin:0 50px 20px 50px");
  Dom.appendChild div (warning_description_header_code_view entry);
  Dom.appendChild div (warning_description_content_code_view entry);
  div
  
let warning_description_page warning_entry plugin_entry =
  let div = Dom_html.createDiv doc in
  let h3 = Dom_html.createH3 doc in
  let h3' = Dom_html.createH3 doc in
  let id = navigation_warning_content_id warning_entry in
  div##setAttribute(Js.string "id", Js.string id);
  div##setAttribute(Js.string "class", Js.string "tab-pane fade");
  h3##innerHTML <- Js.string ("Warning " ^ (string_of_int warning_entry.warning_result.decl.id) ^ " :" );
  h3##setAttribute (Js.string "style", Js.string "margin-left:16px");


  h3'##innerHTML <- Js.string (plugin_entry.plugin_entry_linter_name ^ " : " ^ plugin_entry.plugin_entry_linter_description);
  h3'##setAttribute (Js.string "style", Js.string "margin-left:16px");


  Dom.appendChild div h3;
  Dom.appendChild div h3';
  Dom.appendChild div (Dom_html.createBr doc);
  Dom.appendChild div (warning_description_code_view warning_entry);
  div
    
let main_page warnings_entries plugins_entries =  
  let page = Dom_html.createDiv doc in
  let tabs = navigation_tabs warnings_entries in
  let content = Dom_html.createDiv doc in
  content##setAttribute(Js.string "class", Js.string "tab-content");
  Dom.appendChild content (summary_page tabs content warnings_entries);
  List.iter begin fun warning_entry ->
    let plugin_entry = List.find begin fun plugin_entry ->
      warning_entry.plugin_name = plugin_entry.plugin_entry_plugin_name
      && warning_entry.linter_name = plugin_entry.plugin_entry_linter_name
    end plugins_entries
    in
    Dom.appendChild
      content
      (warning_description_page warning_entry plugin_entry)
  end warnings_entries;
  Dom.appendChild page tabs;
  Dom.appendChild page (Dom_html.createBr doc);
  Dom.appendChild page content;
  page

let json_from_js_var var =
  let (str : Js.js_string Js.t) = Js.Unsafe.variable var in
  Yojson.Basic.from_string (Js.to_string str)
    
let onload _ =
  let warnings_entries =
    database_warning_entries_of_json
      (json_from_js_var Lint_web.warnings_database_var)
  in
  let plugins_entries =
    plugins_database_entries_of_json
      (json_from_js_var Lint_web.plugins_database_var)
  in
  (***)
  List.iter begin fun pe ->
		  Firebug.console##log (Js.string pe.plugin_entry_linter_name)
	    end plugins_entries;
  (***)
  Dom.appendChild (doc##body) (main_page warnings_entries plugins_entries);
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload;
