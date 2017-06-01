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
open Lint_warning_json
open Lint_warning_types

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
             write_pos := !write_pos + 1
       done;
       ignore(Unix.write out_fd write_buf 0 !write_pos : int);
       copy_loop ()
     end in
   copy_loop ()
	     
let dump_js_string out_fd s =
   let i = ref 0 in
   let len = String.length s in
   write_string out_fd "'";
   escaped_copy (fun r rpos rlen ->
     if !i = len then 0 else
       let m = min (len - !i) rlen in
       String.blit s !i r rpos m;
       i := !i + m;
       m
   ) out_fd;
   write_string out_fd "'"

let dump_js_var out_fd var v =
   write_string out_fd ("var " ^ var ^ " = ");
   dump_js_string out_fd v;
   write_string out_fd (";\n")

(***********
let parsing_entries_from_database db =
  Hashtbl.fold begin fun file_name (hash,plugins) acc ->
    acc @ StringMap.fold begin fun plugin_name linters acc ->
      acc @ StringMap.fold begin fun linter_name linter_res acc ->
	{
          file_name = file_name;
          plugin_name = plugin_name;
          linter_name = linter_name;
	  linter_res = linter_res
        } :: acc
      end linters []		     
    end plugins []
	       end db []
  **********)

let output_path =
  "json_output/static/"
		
let javascript_file_name =
  output_path ^ "js/json_var.js"

let json_var_name =
  "json"

let src_viewer_id =
  "code"

let emit_page name page =
  let file = open_out name in
  let fmt = Format.formatter_of_out_channel file in
  pp () fmt page; (* pretty print *)
  close_out file

let html_of_src_viewer src =
  let style =
    "position: absolute;top: 0;right: 0;bottom: 0;left: 0;"
  in
  div
    ~a:[
      a_id src_viewer_id;
      a_style style
    ]
    [pcdata src]
	  
let html_of_ocaml_src fname hash src =
  let css_files = [
    "tmp_warning"
  ] in
  let js_files = [
    "ace_binding";
    "ace"; (* -- *) "ace_file_viewer"; hash  (* --  *)
  ] in
  html
    begin head
      (title (pcdata fname))
      (List.map begin fun src ->
        link ~rel:[`Stylesheet] ~href:("css/" ^ src ^ ".css") ()
       end css_files)
    end
    begin body
      (html_of_src_viewer src
         ::
       List.map begin fun src ->
         script ~a:[a_src (Xml.uri_of_string ("js/" ^ src ^ ".js"))] (pcdata "")
       end js_files)
    end
    
let print fmt path db = (* renommer *)
  let entries =
    raw_entries db
  in
  let json =
    Yojson.Basic.pretty_to_string (json_of_database_warning_entries entries)
  in
  let fd =
    Unix.(openfile javascript_file_name [O_WRONLY;O_CREAT;O_TRUNC] 0o777)
  in
  dump_js_var fd json_var_name json;
  Unix.close fd;
  Hashtbl.iter begin fun file_name (hash,plugin_map) ->

    let hash = Digest.to_hex hash in
    let filterjson =
      entries
      |> List.filter begin fun entry ->
	   entry.file_name = file_name && not entry.warning_result.loc.Location.loc_ghost
	 end
      |> json_of_database_warning_entries	     
      |> Yojson.Basic.pretty_to_string
    in

    
    let fd =
      Unix.(openfile ("json_output/static/js/" ^ hash ^ ".js") [O_WRONLY;O_CREAT;O_TRUNC] 0o777)
    in
    dump_js_var fd json_var_name filterjson;
    Unix.close fd;
    
    
    emit_page
      (output_path ^ hash  ^ ".html")
      (html_of_ocaml_src file_name hash (Lint_utils.read_file file_name))

      
  end db;
		     
