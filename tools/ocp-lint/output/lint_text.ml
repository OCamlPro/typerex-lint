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

open Lint_warning_types
open Lint_db_types

#if OCAML_VERSION >= "4.03"
external isatty : out_channel -> bool = "caml_sys_isatty"

let () =
  if not (isatty stdout) then
#if OCAML_VERSION < "4.05"
    Misc.Color.setup Misc.Color.Never
#else
    Misc.Color.setup (Some Misc.Color.Never)
#endif
#endif

let find_cfg_tmp file config_dep =
  let open Lint_utils in
  try
    let (_, (configs, cfg_tmp)) =
      List.find (fun (file_struct, (_, tmp_cfg)) ->
          file = file_struct.name
        ) config_dep in
    configs, Some cfg_tmp
  with Not_found -> [], None

let print_warning ppf pname lname warning =
  if warning.loc <> Location.none then
    Format.fprintf ppf "%a" Location.print warning.loc;

  Format.fprintf ppf "  Warning %d: %s/%s/%s SEVERITY %d\n"
    warning.decl.id pname lname warning.decl.short_name warning.decl.severity;
  Format.fprintf ppf "  %s" warning.output;
  Format.fprintf ppf "@."

let print_error ppf error =
  let str = match error with
    | Db_error err -> Printf.eprintf "Db_error\n%!"; Lint_db_error.to_string err
    | Plugin_error err ->
      Printf.eprintf "Plugin_error\n%!";
      Lint_plugin_error.to_string err
    | Sempatch_error err ->
      Printf.eprintf "Sem_error\n%!";
      err
    | Ocplint_error str -> Printf.eprintf "OCPL_error\n%!"; str
  in
  Format.fprintf ppf "  %s\n%!" str

let check_flag options =
  try
    bool_of_string
      (Lint_globals.LintConfig.get_option_value options)
  with Not_found -> true

let update_files_linted tbl file cfgs =
  if Hashtbl.mem tbl file then
    let old_cfgs = Hashtbl.find tbl file in
    let new_cfgs = (StringCompat.StringSet.to_list old_cfgs) @ cfgs in
    Hashtbl.replace tbl file (StringCompat.StringSet.of_list new_cfgs)
  else Hashtbl.add tbl file (StringCompat.StringSet.of_list cfgs)

let update_breakdown tbl pname lname version wid =
  let lname = Printf.sprintf "%s.%s" lname version in
  if Hashtbl.mem tbl pname then
    let old_p_htbl = Hashtbl.find tbl pname in
    if Hashtbl.mem old_p_htbl lname then
      let old_l_htbl = Hashtbl.find old_p_htbl lname in
      if Hashtbl.mem old_l_htbl wid then
        let old_w_cpt = Hashtbl.find old_l_htbl wid in
        let new_cpt = old_w_cpt + 1 in
        Hashtbl.replace old_l_htbl wid new_cpt;
        Hashtbl.replace old_p_htbl lname old_l_htbl;
        Hashtbl.replace tbl pname old_p_htbl
      else
        begin
          Hashtbl.add old_l_htbl wid 1;
          Hashtbl.replace old_p_htbl lname old_l_htbl;
          Hashtbl.replace tbl pname old_p_htbl
        end
    else
      begin
        let new_l_htbl = Hashtbl.create 10 in
        Hashtbl.add new_l_htbl wid 1;
        Hashtbl.add old_p_htbl lname new_l_htbl;
        Hashtbl.replace tbl pname old_p_htbl
      end
  else
    begin
      let new_l_htbl = Hashtbl.create 10 in
      let new_p_htbl = Hashtbl.create 10 in
      Hashtbl.add new_l_htbl wid 1;
      Hashtbl.add new_p_htbl lname new_l_htbl;
      Hashtbl.add tbl pname new_p_htbl
    end

let summary ?(oc=stderr) ~master_config ~file_config ~severity ~path
    ~pdetails ~no_db ~db ~db_errors =
  let files_linted = Hashtbl.create 42 in
  let files_cached = ref StringCompat.StringSet.empty in
  let files_linted_errors = ref StringCompat.StringSet.empty in
  let files_cached_errors = ref StringCompat.StringSet.empty in
  let breakdown_linted = Hashtbl.create 42 in
  let breakdown_cached = Hashtbl.create 42 in
  Hashtbl.iter (fun file (hash, pres) ->
      let configs, cfg_opt = find_cfg_tmp file file_config in
      (match cfg_opt with
       | None -> ()
       | Some cfg -> Lint_globals.LintConfig.load_config_tmp master_config cfg);
      StringMap.iter (fun pname lres ->
          let flag = check_flag [pname; "enabled"] in
          if flag then
            StringMap.iter  (fun lname { res_version;
                                         res_source;
                                         res_warnings } ->
                              let flag = check_flag [pname; lname; "enabled"] in
                              let flag_error =
                                if Hashtbl.mem db_errors file then
                                  let err_set = Hashtbl.find db_errors file in
                                  not (ErrorSet.is_empty err_set)
                                else false
                              in
                              if flag && res_source = Analyse then
                                begin
                                  if flag_error then
                                    files_linted_errors :=
                                      StringCompat.StringSet.add
                                        file
                                        !files_linted_errors;
                                  let filters =
                                    Lint_globals.LintConfig.get_option_value
                                      [pname; lname; "warnings"] in
                                  let arr =
                                    Lint_parse_args.parse_options filters in
                                  List.iter
                                    (fun warning ->
                                       let wid = warning.decl.id in
                                       if arr.(wid - 1) &&
                                          warning.decl.severity >= severity then
                                         begin
                                           update_files_linted
                                             files_linted
                                             file
                                             configs;
                                           update_breakdown
                                             breakdown_linted
                                             pname
                                             lname
                                             res_version
                                             wid
                                         end)
                                    res_warnings
                                end
                              else
                              if flag && res_source = Cache then
                                begin
                                  if flag_error then
                                    files_cached_errors :=
                                      StringCompat.StringSet.add
                                        file
                                        !files_cached_errors;
                                  let filters =
                                    Lint_globals.LintConfig.get_option_value
                                      [pname; lname; "warnings"] in
                                  let arr =
                                    Lint_parse_args.parse_options filters in
                                  List.iter
                                    (fun warning ->
                                       let wid = warning.decl.id in
                                       if arr.(wid - 1) &&
                                          warning.decl.severity >= severity then
                                         begin
                                           files_cached :=
                                             StringCompat.StringSet.add
                                               file
                                               !files_cached;
                                           update_breakdown
                                             breakdown_cached
                                             pname
                                             lname
                                             res_version
                                             wid
                                         end)
                                    res_warnings end)
              lres)
        pres)
    db;
  let files_cached_total = StringCompat.StringSet.cardinal !files_cached in
  let warnings_cached_total =
    Hashtbl.fold (fun pname ptbl acc ->
        Hashtbl.fold (fun lname wtbl acc ->
            Hashtbl.fold (fun wid cpt acc ->
                acc + cpt)
              wtbl acc)
          ptbl acc)
      breakdown_cached 0 in
  let files_linted_total = Hashtbl.length files_linted in
  let warnings_linted_total =
    Hashtbl.fold (fun pname ptbl acc ->
        Hashtbl.fold (fun lname wtbl acc ->
            Hashtbl.fold (fun wid cpt acc ->
                acc + cpt)
              wtbl acc)
          ptbl acc)
      breakdown_linted 0 in
  let files_linted_errors_total =
    StringCompat.StringSet.cardinal !files_linted_errors in
  let files_cached_errors_total =
    StringCompat.StringSet.cardinal !files_cached_errors in
  if not no_db then begin
    Printf.fprintf oc "Summary:\n%!";
    Printf.fprintf oc "== Infos ==\n%!";
    Printf.fprintf oc "  * root dir: %s\n%!" !Lint_db.DefaultDB.root;

    Printf.fprintf oc "== Cache Infos ==\n%!";
    if pdetails then begin
      Printf.printf
        "  * %d file(s) were found in cache:\n%!"
        files_cached_total;
      StringCompat.StringSet.iter (Printf.fprintf oc "    - %s\n%!") !files_cached;
    end
    else
      Printf.fprintf oc "  * %d file(s) were found in cache\n%!" files_cached_total;
    Printf.fprintf oc "  * %d warning(s) were found in cache:\n%!"
      warnings_cached_total;
    Hashtbl.iter (fun pname ptbl ->
        let total =
          Hashtbl.fold (fun lname wtbl acc -> acc + 1)
            ptbl 0 in
        let total_w =
          Hashtbl.fold (fun lname wtbl acc ->
              Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl acc)
            ptbl 0 in
        if total > 1 then
          begin
            Printf.fprintf oc "    * %d warning(s) raised by %S\n%!" total_w pname;
            Hashtbl.iter (fun lname wtbl ->
                let total_l =
                  Hashtbl.fold (fun wid cpt acc -> acc + 1) wtbl 0 in
                let total_l_w =
                  Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl 0 in
                if total_l > 1 then
                  begin
                    Printf.fprintf oc "      * %d warning(s) raised by %S\n%!"
                      total_l_w lname;
                    Hashtbl.iter (fun wid cpt ->
                        Printf.fprintf oc "        * %d warning(s) #%i\n%!" cpt wid)
                      wtbl
                  end
                else
                  Hashtbl.iter (fun wid cpt ->
                      Printf.printf
                        "      * %d warning(s) raised by %S/warning #%i\n%!"
                        cpt
                        lname
                        wid)
                    wtbl)
              ptbl
          end
        else
          Hashtbl.iter (fun lname wtbl ->
              let total_l = Hashtbl.fold (fun wid cpt acc -> acc + 1) wtbl 0 in
              let total_l_w =
                Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl 0 in
              if total_l > 1 then
                begin
                  Printf.fprintf oc "      * %d warning(s) raised by %S/%S\n%!"
                    total_l_w pname lname;
                  Hashtbl.iter (fun wid cpt ->
                      Printf.fprintf oc "        * %d warning(s) #%i\n%!" cpt wid)
                    wtbl
                end
              else
                Hashtbl.iter (fun wid cpt ->
                    Printf.printf
                      "      * %d warning(s) raised by %S/%S/warning #%i\n%!"
                      cpt
                      pname
                      lname
                      wid)
                  wtbl)
            ptbl)
      breakdown_cached;
    if pdetails then begin
      Printf.fprintf oc "  * %d files(s) couldn't be linted:\n%!"
        files_cached_errors_total;
      StringCompat.StringSet.iter
        (Printf.fprintf oc "    - %s\n%!") !files_cached_errors;
    end
    else
      Printf.fprintf oc "  * %d files(s) couldn't be linted\n%!"
        files_cached_errors_total
  end;

  Printf.fprintf oc "== New Warnings ==\n%!";
  if pdetails then begin
    Printf.fprintf oc "  * %d file(s) were linted:\n%!" files_linted_total;
    Hashtbl.iter (fun file cfgs ->
        Printf.fprintf oc "    - %s\n%!" file;
        StringCompat.StringSet.iter (fun cfg ->
            let cfg = Filename.concat cfg ".ocplint" in
            Printf.fprintf oc "        * %s\n" cfg)
          cfgs)
      files_linted;
  end
  else
    Printf.fprintf oc "  * %d file(s) were linted\n%!" files_linted_total;
  Printf.fprintf oc "  * %d warning(s) were emitted:\n%!" warnings_linted_total;
  Hashtbl.iter (fun pname ptbl ->
      let total =
        Hashtbl.fold (fun lname wtbl acc -> acc + 1)
          ptbl 0 in
      let total_w =
        Hashtbl.fold (fun lname wtbl acc ->
            Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl acc)
          ptbl 0 in
      if total > 1 then
        begin
          Printf.fprintf oc "    * %d warning(s) raised by %S\n%!" total_w pname;
          Hashtbl.iter (fun lname wtbl ->
              let total_l = Hashtbl.fold (fun wid cpt acc -> acc + 1) wtbl 0 in
              let total_l_w =
                Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl 0 in
              if total_l > 1 then
                begin
                  Printf.fprintf oc "      * %d warning(s) raised by %S\n%!"
                    total_l_w lname;
                  Hashtbl.iter (fun wid cpt ->
                      Printf.fprintf oc "        * %d warning(s) #%i\n%!" cpt wid)
                    wtbl
                end
              else
                Hashtbl.iter (fun wid cpt ->
                    Printf.printf
                      "      * %d warning(s) raised by %S/warning #%i\n%!"
                      cpt
                      lname
                      wid)
                  wtbl)
            ptbl
        end
      else
        Hashtbl.iter (fun lname wtbl ->
            let total_l = Hashtbl.fold (fun wid cpt acc -> acc + 1) wtbl 0 in
            let total_l_w =
              Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl 0 in
            if total_l > 1 then
              begin
                Printf.fprintf oc "      * %d warning(s) raised by %S/%S\n%!"
                  total_l_w pname lname;
                Hashtbl.iter (fun wid cpt ->
                    Printf.fprintf oc "        * %d warning(s) #%i\n%!" cpt wid)
                  wtbl
              end
            else
              Hashtbl.iter (fun wid cpt ->
                  Printf.printf
                    "      * %d warning(s) raised by %S/%S/warning #%i\n%!"
                    cpt
                    pname
                    lname
                    wid)
                wtbl)
          ptbl)
    breakdown_linted;
  if pdetails then begin
    Printf.fprintf oc "  * %d file(s) couldn't be linted:\n%!"
      files_linted_errors_total;
    StringCompat.StringSet.iter
      (Printf.fprintf oc "    - %s\n%!") !files_linted_errors
  end
  else
    Printf.fprintf oc "  * %d file(s) couldn't be linted\n%!"
      files_linted_errors_total

let print fmt master_config file_config severity path db =
  try
    Hashtbl.iter (fun file (hash, pres) ->
        let configs ,cfg_opt = find_cfg_tmp file file_config in
        (match cfg_opt with
         | None -> ()
         | Some cfg ->
           Lint_globals.LintConfig.load_config_tmp master_config cfg);
        let ws =
          StringMap.fold (fun pname lres acc ->
              let flag = check_flag [pname; "enabled"] in
              if flag then
                StringMap.fold (fun lname { res_version; res_warnings } acc ->
                    let flag = check_flag [pname; lname; "enabled"] in
                    if flag then
                      let filters =
                        Lint_globals.LintConfig.get_option_value
                          [pname; lname; "warnings"] in
                      let arr = Lint_parse_args.parse_options filters in
                      List.fold_left
                        (fun acc warning ->
                           if arr.(warning.decl.id - 1) &&
                              warning.decl.severity >= severity then
                             (pname, lname, warning) :: acc
                           else acc) acc res_warnings
                    else acc)
                  lres acc
              else acc)
            pres [] in
        let ws =
          List.sort (fun (_, _, w1) (_, _, w2) ->
              compare w1.loc w2.loc) ws in
        List.iter (fun (pname, lname, w) ->
            print_warning fmt pname lname w) ws)
      db
  with Not_found ->
    Printf.eprintf "Warning: Database contains warnings raised by plugins \
                    that do not exist anymore. Please clean your database.\n%!"

let print_error ppf path db_error =
  let has_error = ref false in
  Hashtbl.iter (fun file error_set ->
      if not (ErrorSet.is_empty error_set) then
        has_error := true
    ) db_error;
  if !has_error then
    begin
      Format.fprintf ppf "=== Errors ===\n%!";
      Hashtbl.iter (fun file error_set ->
          if not (ErrorSet.is_empty error_set) then
            begin
              Format.fprintf ppf "%S:\n%!" file;
              ErrorSet.iter (print_error ppf) error_set
            end)
        db_error;
      Format.fprintf ppf "==============\n%!"
    end

let print_only_new fmt path db =
  Hashtbl.iter (fun file (hash, pres) ->
      StringMap.iter (fun pname lres ->
          StringMap.iter  (fun lname { res_source; res_warnings } ->
              if res_source = Analyse then
                List.iter (print_warning fmt pname lname) res_warnings)
            lres)
        pres)
    db

let verbose_info fmt db =
  Hashtbl.iter (fun file (hash, pres) ->
      let triggered_plugin =
        StringMap.filter (fun pname lres ->
            StringMap.exists (fun lname { res_warnings } ->
                let plugin_flag = check_flag [pname; "enabled"] in
                let linter_flag = check_flag [pname; lname; "enabled"] in
                let filters =
                  Lint_globals.LintConfig.get_option_value
                    [pname; lname; "warnings"] in
                let arr = Lint_parse_args.parse_options filters in
                let warnings_activated =
                  List.filter (fun warning ->
                      arr.(warning.decl.id - 1)) res_warnings in
                let warning_flag = warnings_activated <> [] in
                warning_flag && plugin_flag && linter_flag)
              lres)
          pres in
      let triggered_plugin_len = StringMap.cardinal triggered_plugin in
      let plugin_len = StringMap.cardinal pres in
      if triggered_plugin_len = 0 then
        Format.fprintf fmt "\027[32m%S\027[m: [%i / %i]\n%!"
          file triggered_plugin_len plugin_len
      else
        Format.fprintf fmt "\027[31m%S\027[m: [%i / %i]\n%!"
          file triggered_plugin_len plugin_len)
    db

let debug_db db =
  Hashtbl.iter (fun file (hash, pres) ->
      let ws =
        StringMap.fold (fun pname lres acc ->
            StringMap.fold (fun lname { res_warnings } acc ->
                List.fold_left
                  (fun acc warning -> (pname, lname, warning) :: acc) acc
                  res_warnings)
              lres acc)
          pres [] in
      let ws =
        List.sort (fun (_, _, w1) (_, _, w2) ->
            compare w1.loc w2.loc) ws in
      List.iter (fun (pname, lname, w) ->
          print_warning Format.std_formatter pname lname w) ws)
    db
