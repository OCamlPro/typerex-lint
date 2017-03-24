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

open Tyxml.Html
open Omd

(* let _ = SimpleConfig.LowLevel.set_string_wrappers cl (to_string) (of_string) *)

let emit_page name page =
  Printf.printf "Generating: %s\n" name ;
  let file_handle = open_out name in
  let fmt = Format.formatter_of_out_channel file_handle in
  pp () fmt page;
  close_out file_handle

let read_warning_data filename =
  let file_path = Filename.concat "tools/ocp-lint-doc/examples" filename in
  if Sys.file_exists file_path then
    let cnt = Lint_utils.read_file file_path in
    if cnt = "" then None
    else Some (code [pcdata cnt])
  else
    None

let read_example plugin linter id =
  let filename = Printf.sprintf "%s.%s.%i.ml" plugin linter id in
  read_warning_data filename

let read_warning plugin linter id =
  let filename = Printf.sprintf "%s.%s.%i.warn" plugin linter id in
  read_warning_data filename

let make_option_tab pname lname =
  let opt =
    Lint_globals.Config.get_linter_options_details pname lname in
  let thead =
    thead [tr [th [pcdata "Command line arg"];
               th [pcdata "Details"];
               th [pcdata "Default value"]]] in
  let rows = List.map (fun (name, help, default) ->
      let is_enabled_opt =
        let re = Str.regexp_string "enabled" in
        try ignore ((Str.search_forward re name 0)); true
        with Not_found -> false in
      let name =
        if is_enabled_opt then
          Printf.sprintf "--{enable,disable}-%s.%s" pname lname
        else "--" ^ name in
      let name = Str.global_replace (Str.regexp_string "_") "-" name in
      let td_name = td [i [pcdata name]] in
      let td_help = td [pcdata help] in
      let td_def = td [pcdata default] in
      tr [td_name; td_help; td_def]
    ) opt in
  table ~a:[a_class ["table-striped table-bordered"]] ~thead rows

let linter_page_div pname lname linter =
  let open Lint_warning_decl in
  let open Lint_warning_types in
  let module Linter = (val linter : Lint_types.LINT) in
  let name = Linter.name in
  let version = Linter.version in
  let details = Linter.details in
  let name_h1 = h1 [pcdata name] in
  let name = Printf.sprintf " : %s " lname in
  let name_p = p [b [pcdata "Shortname" ]; pcdata name] in
  let details = Printf.sprintf " : %s " details in
  let details_p = p [b [pcdata "Description"]; pcdata details] in
  let version = Printf.sprintf " : %s " version in
  let version_p = p [b [pcdata "Version"]; pcdata version] in
  let opt_h2 = h2 [pcdata "Options"] in
  let opt_tab = make_option_tab pname lname in
  let warn =
    WarningDeclaration.fold (fun wdecl acc ->
        let title_str = Printf.sprintf "Warning %i" wdecl.id in
        let name_h3 = h3 [pcdata title_str] in
        let name = Printf.sprintf " : %s " wdecl.short_name in
        let name_p = p [b [pcdata "Name" ]; pcdata name] in
        let severity = Printf.sprintf " : %i" wdecl.severity in
        let severity_p = p [b [pcdata "Severity"]; pcdata severity] in
        let message = Printf.sprintf " : %s" wdecl.message in
        let message_p = p [b [pcdata "Message"]; pcdata message] in
        let example_txt = "Code example that triggers this warnings : " in
        let example_p = p [pcdata example_txt] in
        let example_code = read_example pname lname wdecl.id in
        let div = match example_code with
          | Some c ->
            let warning_code = read_warning pname lname wdecl.id in
            begin
              match warning_code with
              | None -> div [ name_h3; name_p; severity_p; message_p; example_p ; pre [c] ]
              | Some c2 -> div [ name_h3; name_p; severity_p; message_p; example_p ; pre [c]; pre [c2] ]
            end
          | None ->
            div [ name_h3; name_p; severity_p; message_p; ] in
        div::acc)
      Linter.wdecls [] in
  let warn_div = div (List.rev warn) in
  let warn_h2 = h2 [pcdata "Warnings"] in
  div
    ~a:[a_class ["content"]; a_id (Linter.short_name ^ "_div")]
    [name_h1; name_p; details_p; version_p; opt_h2; opt_tab; warn_h2; warn_div]

let emit_plugin_page plugin linters =
  let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
  let short_name = Plugin.short_name in
  let name = Plugin.name in
  let details = Plugin.details in
  let name_h1 = h1 [pcdata name] in
  let name = Printf.sprintf " : %s" name in
  let name_p = p [b [pcdata "Name"]; pcdata name] in
  let details = Printf.sprintf " : %s" details in
  let details_p = p [b [pcdata "Description"]; pcdata details] in
  let linters_li, linters_div =
    Lint_map.fold (fun lname lint (accli, accdiv) ->
        let handler_str = Printf.sprintf "handler(%S)" lname in
        let li_id = lname ^ "_li" in
        let li =
          li
            ~a:[a_onclick handler_str; a_id li_id]
            [a ~a:[a_class ["sub-menu"]]
               [pcdata lname]] in
        let div = linter_page_div short_name lname lint in
        li::accli, div::accdiv
      )
      linters ([], []) in
  let linters_ul = (List.rev linters_li) in
  let linters_list_div = div [p [b [pcdata "Linters"]]; ul linters_ul] in
  let linters_div = List.rev linters_div in
  let dom = [name_h1; name_p; details_p; linters_list_div] in
  (div ~a:[a_class ["content"]; a_id (short_name ^ "_div")] dom)::linters_div,
  linters_ul

let main_script =
  script
    ~a:[a_src (Xml.uri_of_string "js/make_doc.js")]
    (pcdata "")

let _ =
  let open Lint_warning_decl in
  let plugins_li, linters_ul, plugins_div =
    Hashtbl.fold (fun plugin linters (accli, accul, accdiv) ->
        let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
        let lname = Plugin.short_name in
        let handler_str = Printf.sprintf "handler(%S)" lname in
        let li_id = lname ^ "_li" in
        let pli =
          li
            ~a:[a_onclick handler_str; a_id li_id]
            [a [pcdata lname]] in
        let pdiv, linter_ul = emit_plugin_page plugin linters in
        pli::accli, linter_ul::accul, pdiv::accdiv)
      Lint_globals.plugins ([], [], []) in
  let plugins_li = List.rev plugins_li in
  let sidebar = List.combine plugins_li (List.rev linters_ul) in
  let sidebar_li =
    List.fold_left (fun acc (pli, llis) -> (pli::llis) @ acc) [] sidebar in
  let plugins_div = List.flatten (List.rev plugins_div) in
  let plugins_div = div ~a:[a_id "page-content-wrapper"] plugins_div in
  let title = title (pcdata "ocp-lint: plugins") in
  let sidebar_ul = ul ~a:[a_class ["sidebar-nav"]] sidebar_li in
  let sidebar_list_div = div ~a:[a_id "sidebar-wrapper"] [sidebar_ul] in
  let dom = div ~a:[a_id "wrapper"] [sidebar_list_div; plugins_div] in
  let page =
    html (head title
            [link
               ~rel:[`Stylesheet]
               ~href:"css/bootstrap.min.css" ();
             link
               ~rel:[`Stylesheet]
               ~href:"css/ocplint.css" ();
             link
               ~rel:[`Stylesheet]
               ~href:"css/simple-sidebar.css" ();])
      (body [dom; main_script]) in
  emit_page "tools/ocp-lint-doc/index.html" page
