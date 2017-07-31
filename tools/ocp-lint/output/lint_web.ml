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
open Lint_web_analysis_info

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

let js_string_of_json_var var v =
  (* todo changer implantation *)
  let tmp_file_name =
    "tools/ocp-lint-web/tmp_js_var"
  in
  let fd =
    Unix.(openfile tmp_file_name [O_WRONLY;O_CREAT;O_TRUNC] 0o777)
  in
  dump_js_var fd var (Yojson.Basic.to_string v);
  Unix.close fd;
  Lint_utils.read_file tmp_file_name

let emit_page name page =
  let file = open_out name in
  let fmt = Format.formatter_of_out_channel file in
  pp () fmt page; (* pretty print *)
  close_out file

let path_of_dir_list dirs =
  List.fold_left begin fun acc dir ->
    Filename.concat acc dir
  end (List.hd dirs) (List.tl dirs)

let clear_dir dir =
  Lint_utils.iter_files begin fun filename ->
    let fullname = Filename.concat dir filename in
    FileGen.remove (FileGen.of_string fullname)
  end dir ~recdir:false

let find_cfg_tmp file config_dep =
  let open Lint_utils in
  try
    let (_, (configs, cfg_tmp)) =
      List.find begin fun (file_struct, (_, tmp_cfg)) ->
        file = file_struct.name
      end config_dep
    in
    configs, Some cfg_tmp
  with
    Not_found -> [], None

let check_flag options =
  try
    bool_of_string (Lint_globals.Config.get_option_value options)
  with
    Not_found -> true

let plugin_is_enabled plugin_name =
  check_flag [plugin_name; "enabled"]

let linter_is_enabled plugin_name linter_name =
  check_flag [plugin_name; linter_name; "enabled"]

let warnings_activations plugin_name linter_name =
  let opt = [plugin_name; linter_name; "warnings"] in
  Lint_parse_args.parse_options (Lint_globals.Config.get_option_value opt)

let make_plugins_linters_info plugins_tbl =
  Hashtbl.fold begin fun plugin linters (plugin_acc,linter_acc) ->
    let module Plugin = (val plugin : Lint_plugin_types.PLUGIN) in
    if Plugin.enable then
      let plugin_info =
        {
          plugin_name = Plugin.short_name;
          plugin_description = Plugin.details;
        }
      in
      plugin_info :: plugin_acc,
      Lint_map.fold begin fun lname lint linter_acc ->
        let module Linter = (val lint : Lint_types.LINT) in
        if Linter.enable then
          let linter_info =
            {
              linter_plugin = plugin_info;
              linter_name = Linter.short_name;
              linter_description = Linter.details;
            }
          in
          linter_info :: linter_acc
        else
          linter_acc
      end linters linter_acc
    else
      plugin_acc, linter_acc
  end plugins_tbl ([],[])

let make_files_warnings_info master_config file_config linters_info db =
  let _, files_info, warnings_info =
    Hashtbl.fold begin
      fun file_name (hash,plugin_map) (id,file_acc,warning_acc) ->
      let configs ,cfg_opt = find_cfg_tmp file_name file_config in
      begin match cfg_opt with
         | None -> ()
         | Some cfg -> Lint_globals.Config.load_config_tmp master_config cfg
      end;
      let file_info =
        {
          file_name = file_name;
          file_hash = hash;
          file_lines_count = Lint_utils.lines_count_of_file file_name;
        }
      in
      let id, warnings_info =
        StringMap.fold begin fun plugin_name linter_map (id,warning_acc) ->
        if plugin_is_enabled plugin_name then
          StringMap.fold begin fun linter_name linter_result (id,warning_acc) ->
            if linter_is_enabled plugin_name linter_name then
              let arr = warnings_activations plugin_name linter_name in
              List.fold_left begin fun (id,warning_acc) warning_result ->
                if arr.(warning_result.decl.id - 1) then
                  let linter_info =
                    List.find begin fun linter ->
                      linter.linter_name = linter_name
                      && linter.linter_plugin.plugin_name = plugin_name
                    end linters_info
                  in
                  let warning_info =
                    {
                      warning_id = id;
                      warning_file = file_info;
                      warning_linter = linter_info;
                      warning_type = warning_result;
                    }
                  in
                  id + 1, warning_info :: warning_acc
                else
                  id, warning_acc
              end (id, warning_acc) linter_result.res_warnings
            else
              id, warning_acc
          end linter_map (id, warning_acc)
        else
          id, warning_acc
        end plugin_map (id, warning_acc)
      in
      id, file_info :: file_acc, warnings_info
    end db (1, [],[])
  in
  files_info, warnings_info

let make_errors_info files_info db_error =
  let _, errors =
    Hashtbl.fold begin fun file error_set (id, acc) ->
      if not (ErrorSet.is_empty error_set) then
        let file_info =
          List.find begin fun file_info ->
            file_info.file_name = file
          end files_info
        in
        ErrorSet.fold begin fun error (id, acc) ->
          let error_info =
            {
              error_id = id;
              error_file = file_info;
              error_type = error;
            }
          in
          id + 1, error_info :: acc
        end error_set (id, acc)
      else
        id, acc
  end db_error (1, [])
  in
  errors

let make_analysis_info
      time path master_config file_config db db_error plugins_tbl =
  let plugins_info, linters_info =
    make_plugins_linters_info plugins_tbl
  in
  let files_info, warnings_info =
    make_files_warnings_info master_config file_config linters_info db
  in
  let errors_info =
    make_errors_info files_info db_error
  in
  {
    files_info = files_info;
    plugins_info = plugins_info;
    linters_info = linters_info;
    warnings_info = List.rev warnings_info; (* for id sort *)
    errors_info = List.rev errors_info; (* for id sort *)
    analysis_root = path;
    analysis_date = time;
  }

let output_path =
  path_of_dir_list ["tools"; "ocp-lint-web"; "static"]

let path_of_js fname =
  Filename.concat "js" (fname ^ ".js")

let path_of_css fname =
  Filename.concat "css" (fname ^ ".css")

let path_of_html fname =
  fname ^ ".html"

let analysis_info_file =
  "ocp_lint_web_analysis_info"

let analysis_info_var =
  "analysis_info_json"

let warnings_info_var =
  "info_warnings_json"

let web_code_viewer_id =
  "ocp-code-viewer"

let web_code_loading_animation_id =
  "ocp-code-loading"

let html_of_index =
  let css_files = [
    "dataTables.min";
    "bootstrap.min";
    "adjustment_dataTables";
    "adjustment_bootstrap";
    "ocp_lint_web"
  ] in
  let js_files = [
    analysis_info_file;
    "ocp_lint_web";
    "jquery.min";
    "bootstrap.min";
    "d3";
    "d3pie.min";
    "data_table";
  ] in
  html
    begin head
      (title (pcdata "index"))
      (List.map begin fun src ->
        link ~rel:[`Stylesheet] ~href:(path_of_css src) ()
       end css_files)
    end
    begin body
      (List.map begin fun src ->
         script ~a:[a_src (Xml.uri_of_string (path_of_js src))] (pcdata "")
       end js_files)
    end

let code_viewer src =
  div
    ~a:[
      a_id web_code_viewer_id;
      a_style "display: none;"
    ]
    [pcdata src]

let loading_animation () =
  div
    ~a:[
      a_id web_code_loading_animation_id;
      a_class ["loading-animation"];
    ]
    []

let html_of_ocaml_src file_info warnings_info src =
  let css_files = [
    "adjustment_ace";
    "ocp_lint_web"
  ]
  in
  let js_files = [
    "ace";
    "ocp_lint_web_codeviewer";
  ]
  in
  let js_warnings_info_var =
    js_string_of_json_var
      warnings_info_var
      (json_of_warnings_info warnings_info)
  in
  html
    begin head
      (title (pcdata file_info.file_name))
      (List.map begin fun src ->
        link ~rel:[`Stylesheet] ~href:(path_of_css src) ()
       end css_files)
    end
    begin body (
      loading_animation ()
      :: code_viewer src
      :: script (cdata_script js_warnings_info_var)
      :: List.map begin fun src ->
           script ~a:[a_src (Xml.uri_of_string (path_of_js src))] (pcdata "")
          end js_files
    ) end

let generate_web_files fmt master_config file_config path db db_error =
  let time = Unix.localtime (Unix.time ()) in
  let analysis_info =
    make_analysis_info
      time
      path
      master_config
      file_config
      db
      db_error
      Lint_globals.plugins
  in
  let json_analysis =
    Yojson.Basic.pretty_to_string
      (json_of_analysis_info analysis_info)
  in
  clear_dir output_path;
  dump_js_var_file
    (Filename.concat output_path (path_of_js analysis_info_file))
    analysis_info_var
    json_analysis;
  let warnings_info_file =
    Lint_utils.group_by begin fun warning_info ->
      warning_info.warning_file
    end analysis_info.warnings_info
  in
  let errors_info_file =
    Lint_utils.group_by begin fun error_info ->
      error_info.error_file
    end analysis_info.errors_info
  in
  let files_info_with_warn_or_err =
    let rec aux lw le =
      match lw, le with
      | [], [] ->
         []
      | lw, [] ->
         List.map (fun (file,warns) -> (file,warns,[])) lw
      | [], le ->
         List.map (fun (file,errs) -> (file,[],errs)) le
      | (file_w,_) :: tlw, (file_e,errs) :: tle when file_w < file_e ->
         (file_e,[],errs) :: aux lw tle
      | (file_w,warns) :: tlw, (file_e,_) :: tle when file_w > file_e ->
         (file_w,warns,[]) :: aux tlw le
      | (file,warns) :: tlw, (_,errs) :: tle ->
         (file,warns,errs) :: aux tlw tle
    in
    aux warnings_info_file errors_info_file
  in
  List.iter begin fun (file_info, warnings_info, errors_info) ->
    let html_src =
      html_of_ocaml_src
        file_info
        warnings_info
        (Lint_utils.read_file file_info.file_name)
    in
    let page_name = generated_static_page_of_file file_info in
    emit_page
      (Filename.concat output_path (path_of_html page_name))
      html_src
  end files_info_with_warn_or_err;
  emit_page
    (Filename.concat output_path (path_of_html "index"))
    html_of_index
