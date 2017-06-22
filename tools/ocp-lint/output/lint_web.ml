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
open Lint_db_types
open Lint_warning_types
open Lint_web_warning
open Lint_web_plugin

let write_string fd str =
   let str = Bytes.of_string str in
   ignore (Unix.write fd str 0 (Bytes.length str))

let escaped_copy reader out_fd =
  let read_buf = Bytes.create 4096 in
  let write_buf = Bytes.create (4096*4) in
  let conv = "0123456789abcdef" in
  let rec copy_loop () =
    let read_size = reader read_buf 0 (Bytes.length read_buf) in
    if read_size > 0 then begin
       let write_pos = ref 0 in
       let push c =
         Bytes.set write_buf (!write_pos+0) '\\';
         Bytes.set write_buf (!write_pos+1) c;
         write_pos := !write_pos + 2 in
       for i = 0 to read_size - 1 do
         match Bytes.get read_buf i with
         | '\000' when i < read_size - 1 &&
             (Bytes.get read_buf (i + 1) < '0'
              || Bytes.get read_buf (i + 1) > '9') ->
             push '0'
         | '\b' -> push 'b'
         | '\t' ->  push 't'
         | '\n' ->  push 'n'
         | '\012' -> push 'f'
         | '\\' -> push '\\'
         | '\r' -> push 'r'
         | '\'' -> push '\''
         | ('\000' .. '\031'  | '\127' .. '\255') as c  ->
             let c = Char.code c in
             Bytes.set write_buf (!write_pos+0) '\\';
             Bytes.set write_buf (!write_pos+1)  'x';
             Bytes.set write_buf (!write_pos+2)  conv.[c lsr 4];
             Bytes.set write_buf (!write_pos+3)  conv.[c land 0xf];
             write_pos := !write_pos + 4
         | c ->
             Bytes.set write_buf (!write_pos) c;
             incr write_pos
       done;
       ignore (Unix.write out_fd write_buf 0 !write_pos : int);
       copy_loop ()
     end in
  copy_loop ()

let dump_js_string out_fd s =
   let i = ref 0 in
   let len = String.length s in
   write_string out_fd "'";
   escaped_copy begin fun r rpos rlen ->
     if !i = len then 0 else
       let m = min (len - !i) rlen in
       String.blit s !i r rpos m;
       i := !i + m;
       m
   end out_fd;
   write_string out_fd "'"

let dump_js_var out_fd var v =
   write_string out_fd ("var " ^ var ^ " = ");
   dump_js_string out_fd v;
   write_string out_fd ";\n"

let dump_js_var_file fname var v =
  let fd =
    Unix.(openfile fname [O_WRONLY;O_CREAT;O_TRUNC] 0o777)
  in
  dump_js_var fd var v;
  Unix.close fd

let emit_page name page =
  let file = open_out name in
  let fmt = Format.formatter_of_out_channel file in
  pp () fmt page; (* pretty print *)
  close_out file

let warnings_database_raw_entries db =
  let _,entries =
    Hashtbl.fold begin fun file_name (hash, plugin_map) acc ->
      StringMap.fold begin fun plugin_name linter_map acc ->
        StringMap.fold begin fun linter_name linter_result acc ->
	  List.fold_left begin fun (id,acc) warning_result ->
	    id + 1, {
	      id = id;
	      file_name = file_name;
	      hash = hash;
	      lines_count = Lint_utils.lines_count_of_file file_name;
	      plugin_name = plugin_name;
	      linter_name = linter_name;
	      linter_version = linter_result.res_version;
	      warning_result = warning_result;
	    } :: acc	       
	  end acc linter_result.res_warnings	
        end linter_map acc					      
      end plugin_map acc 
    end db (0,[])
  in entries
	    
let plugins_database_raw_entries db =
  Hashtbl.fold begin fun plugin linters acc ->
    let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
    let plugin_name = Plugin.short_name in
    Lint_map.fold begin fun lname lint acc ->
      let module Linter = (val lint : Lint_types.LINT) in
      let linter_name = Linter.short_name in
      {
        plugin_entry_plugin_name = plugin_name;
	plugin_entry_plugin_description = "";
  	plugin_entry_linter_name = linter_name;
	plugin_entry_linter_description = "";
      } :: acc
    end linters acc
  end db []
	       
(* todo move in lint_web_warning *)
let group_by clss lst = (*** todo changer implantation ***)
  let rec aux acc = function
    | [] -> acc
    | (cx, x) :: y -> (*** todo changer ***)
       begin match acc with
	     | (cx', x') :: y' when cx = cx' ->
		aux ((cx, x :: x') :: y') y
	     | _ ->
		aux ((cx, [x]) :: acc) y
       end
  in
  lst
  |> List.map (fun x -> clss x, x) (*** ***)
  |> List.sort (fun (c,_) (c',_) -> Pervasives.compare c c')
  |> aux []
(*                             *)
	    
let output_path =
  "tools/ocp-lint-web/static/"

let full_path_of_js fname =
  "js/" ^ fname ^ ".js"

let full_path_of_css fname =
  "css/" ^ fname ^ ".css"

let warnings_database_file =
  "ocp_lint_web_json_warnings_database"

let warnings_database_var =
  "warnings_json"

let plugins_database_file =
  "ocp_lint_web_json_plugins_database"

let plugins_database_var =
  "plugins_json"

let web_static_gen_file file_hash =
  "ocp_lint_web_generated_" ^ (Digest.to_hex file_hash)

let html_of_index =
  let css_files = [
    "dataTables.min";
    "bootstrap.min";
    "adjustment_dataTables";
    "adjustment_bootstrap";
  ] in
  let js_files = [
    warnings_database_file;
    plugins_database_file;
    "ace"; (*** tmp ***)
    "ocp_lint_web";
    "jquery.min"; (*** 11 ***)
    "bootstrap.min";
    "jquery-1.12.4"; (*** 11 ***)
    "jquery.dataTables.min";
    "data_table";
  ] in
  html
    begin head
      (title (pcdata "index"))
      (List.map begin fun src ->
        link ~rel:[`Stylesheet] ~href:(full_path_of_css src) ()
       end css_files)
    end
    begin body
      (List.map begin fun src ->
         script ~a:[a_src (Xml.uri_of_string (full_path_of_js src))] (pcdata "")
       end js_files)
    end

let html_of_src_viewer src =
  let src_viewer_id = (* todo global pour etre utiliser dans js of ocaml *)
    "ocp-code-viewer"
  in
  let style =
    "position: absolute;top: 0;right: 0;bottom: 0;left: 0;"
  in
  div
    ~a:[
      a_id src_viewer_id;
      a_style style;
    ]
    [pcdata src]

let html_of_ocaml_src fname hash src =
  let css_files = [
    "adjustment_ace";
  ] in
  let js_files = [
    "ace";
    "ocp_lint_web_codeviewer";
    (* -- *)
    (web_static_gen_file hash);
    (* --  *)
  ] in
  html
    begin head
      (title (pcdata fname))
      (List.map begin fun src ->
        link ~rel:[`Stylesheet] ~href:(full_path_of_css src) ()
       end css_files)
    end
    begin body
      (html_of_src_viewer src
         ::
       List.map begin fun src ->
         script ~a:[a_src (Xml.uri_of_string (full_path_of_js src))] (pcdata "")
       end js_files)
    end
       
let print fmt path db = (* renommer *)
  let warnings_entries = warnings_database_raw_entries db in
  let json_warnings =
    Yojson.Basic.pretty_to_string
      (json_of_database_warning_entries warnings_entries)
  in
  let plugins_entries = plugins_database_raw_entries Lint_globals.plugins in
  let json_plugins =
    Yojson.Basic.pretty_to_string
      (json_of_plugins_database_entries plugins_entries)
  in
  dump_js_var_file
    (output_path ^ "js/" ^ warnings_database_file ^ ".js") (****)
    warnings_database_var
    json_warnings;
  dump_js_var_file
    (output_path ^ "js/" ^ plugins_database_file ^ ".js") (****)
    plugins_database_var
    json_plugins;
  let warnings_entries_filename =
    group_by begin fun warning_entry ->
      (warning_entry.file_name, warning_entry.hash)
    end warnings_entries
  in
  List.iter begin fun ((filename, hash), entries) ->
    let json_warnings_filename =
      entries
      |> json_of_database_warning_entries
      |> Yojson.Basic.pretty_to_string
    in
    (****)
    dump_js_var_file
      (output_path ^ "js/" ^ (web_static_gen_file hash) ^ ".js")
      "json"
      json_warnings_filename;
    (****)
    emit_page
      (output_path ^ (web_static_gen_file hash) ^ ".html")
      (html_of_ocaml_src filename hash (Lint_utils.read_file filename))
  end warnings_entries_filename;
  emit_page
    (output_path ^ "index.html")
    html_of_index
