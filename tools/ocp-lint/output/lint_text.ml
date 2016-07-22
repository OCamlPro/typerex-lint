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
      Sempatch.Failure.to_string err
    | Ocplint_error str -> Printf.eprintf "OCPL_error\n%!"; str
  in
  Format.fprintf ppf "  %s\n%!" str

let check_flag options =
  try
    bool_of_string
      (Lint_globals.Config.get_option_value options)
  with Not_found -> true

let update_breakdown tbl pname lname wid =
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
        (Hashtbl.add old_l_htbl wid 1;
         Hashtbl.replace old_p_htbl lname old_l_htbl;
         Hashtbl.replace tbl pname old_p_htbl)
    else
      (let new_l_htbl = Hashtbl.create 10 in
       Hashtbl.add new_l_htbl wid 1;
       Hashtbl.add old_p_htbl lname new_l_htbl;
       Hashtbl.replace tbl pname old_p_htbl)
  else
    (let new_l_htbl = Hashtbl.create 10 in
     let new_p_htbl = Hashtbl.create 10 in
     Hashtbl.add new_l_htbl wid 1;
     Hashtbl.add new_p_htbl lname new_l_htbl;
     Hashtbl.add tbl pname new_p_htbl)

let summary severity path db db_errors =
  let files_linted = ref StringCompat.StringSet.empty in
  let files_cached = ref StringCompat.StringSet.empty in
  let files_linted_errors = ref StringCompat.StringSet.empty in
  let files_cached_errors = ref StringCompat.StringSet.empty in
  let breakdown_linted = Hashtbl.create 42 in
  let breakdown_cached = Hashtbl.create 42 in
  Hashtbl.iter (fun file (hash, pres) ->
      if Lint_utils.(is_in_path !Lint_db.DefaultDB.root file path) then
        StringMap.iter (fun pname lres ->
            let flag = check_flag [pname; "enabled"] in
            if flag then
              StringMap.iter  (fun lname (source, _opt, ws) ->
                  let flag = check_flag [pname; lname; "enabled"] in
                  let flag_error =
                    if Hashtbl.mem db_errors file then
                      let err_set = Hashtbl.find db_errors file in
                      not (ErrorSet.is_empty err_set)
                    else false
                  in
                  if flag && source = Analyse then
                    begin
                      if flag_error then
                        files_linted_errors :=
                          StringCompat.StringSet.add file !files_linted_errors;
                      let filters =
                        Lint_globals.Config.get_option_value
                          [pname; lname; "warnings"] in
                      let arr = Lint_parse_args.parse_options filters in
                      List.iter
                        (fun warning ->
                           let wid = warning.decl.id in
                           if arr.(wid - 1) &&
                              warning.decl.severity >= severity then
                             (files_linted :=
                                StringCompat.StringSet.add file !files_linted;
                              update_breakdown
                                breakdown_linted pname lname wid))
                        ws
                    end
                  else
                  if flag && source = Cache then
                    begin
                      if flag_error then
                        files_cached_errors :=
                          StringCompat.StringSet.add file !files_cached_errors;
                      let filters =
                        Lint_globals.Config.get_option_value
                          [pname; lname; "warnings"] in
                      let arr = Lint_parse_args.parse_options filters in
                      List.iter
                        (fun warning ->
                           let wid = warning.decl.id in
                           if arr.(wid - 1) &&
                              warning.decl.severity >= severity then
                             (files_cached :=
                                StringCompat.StringSet.add file !files_cached;
                              update_breakdown
                                breakdown_cached pname lname wid))
                        ws end)
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
  let files_linted_total = StringCompat.StringSet.cardinal !files_linted in
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
  Printf.printf "Summary:\n%!";
  Printf.printf "== Infos ==\n%!";
  Printf.printf "  * root dir: %s\n%!" !(Lint_db.DefaultDB.root);

  Printf.printf "== Cache Infos ==\n%!";
  Printf.printf "  * %d file(s) were found in cache:\n%!" files_cached_total;
  StringCompat.StringSet.iter (Printf.printf "    - %s\n%!") !files_cached;
  Printf.printf "  * %d warning(s) were found in cache:\n%!"
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
        (Printf.printf "    * %d warnings(s) raised by %S\n%!" total_w pname;
         Hashtbl.iter (fun lname wtbl ->
           let total_l = Hashtbl.fold (fun wid cpt acc -> acc + 1) wtbl 0 in
           let total_l_w = Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl 0 in
           if total_l > 1 then
             (Printf.printf "      * %d warnings(s) raised by %S\n%!"
                total_l_w lname;
              Hashtbl.iter (fun wid cpt ->
                  Printf.printf "        * %d Warning %i\n%!" cpt wid)
                wtbl)
           else
             Hashtbl.iter (fun wid cpt ->
                 Printf.printf "      * %d warnings(s) raised by %S/%i\n%!"
                   cpt lname wid)
               wtbl)
           ptbl)
      else
        Hashtbl.iter (fun lname wtbl ->
            let total_l = Hashtbl.fold (fun wid cpt acc -> acc + 1) wtbl 0 in
            let total_l_w =
              Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl 0 in
            if total_l > 1 then
              (Printf.printf "      * %d warnings(s) raised by %S/%S\n%!"
                 total_l_w pname lname;
               Hashtbl.iter (fun wid cpt ->
                   Printf.printf "        * %d Warning %i\n%!" cpt wid)
                 wtbl)
            else
              Hashtbl.iter (fun wid cpt ->
                  Printf.printf "      * %d warnings(s) raised by %S/%S/%i\n%!"
                    cpt pname lname wid)
                wtbl)
          ptbl)
    breakdown_cached;
  Printf.printf "  * error(s) on %d file(s) were found in cache:\n%!"
    files_cached_errors_total;
  StringCompat.StringSet.iter
    (Printf.printf "    - %s\n%!") !files_cached_errors;

  Printf.printf "== New Warnings ==\n%!";
  Printf.printf "  * %d file(s) were linted:\n%!" files_linted_total;
  StringCompat.StringSet.iter (Printf.printf "    - %s\n%!") !files_linted;
  Printf.printf "  * %d warning(s) were emitted:\n%!" warnings_linted_total;
  Hashtbl.iter (fun pname ptbl ->
      let total =
        Hashtbl.fold (fun lname wtbl acc -> acc + 1)
          ptbl 0 in
      let total_w =
        Hashtbl.fold (fun lname wtbl acc ->
            Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl acc)
          ptbl 0 in
      if total > 1 then
        (Printf.printf "    * %d warnings(s) raised by %S\n%!" total_w pname;
         Hashtbl.iter (fun lname wtbl ->
           let total_l = Hashtbl.fold (fun wid cpt acc -> acc + 1) wtbl 0 in
           let total_l_w = Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl 0 in
           if total_l > 1 then
             (Printf.printf "      * %d warnings(s) raised by %S\n%!"
                total_l_w lname;
              Hashtbl.iter (fun wid cpt ->
                  Printf.printf "        * %d Warning %i\n%!" cpt wid)
                wtbl)
           else
             Hashtbl.iter (fun wid cpt ->
                 Printf.printf "      * %d warnings(s) raised by %S/%i\n%!"
                   cpt lname wid)
               wtbl)
           ptbl)
      else
        Hashtbl.iter (fun lname wtbl ->
            let total_l = Hashtbl.fold (fun wid cpt acc -> acc + 1) wtbl 0 in
            let total_l_w =
              Hashtbl.fold (fun wid cpt acc -> acc + cpt) wtbl 0 in
            if total_l > 1 then
              (Printf.printf "      * %d warnings(s) raised by %S/%S\n%!"
                 total_l_w pname lname;
               Hashtbl.iter (fun wid cpt ->
                   Printf.printf "        * %d Warning %i\n%!" cpt wid)
                 wtbl)
            else
              Hashtbl.iter (fun wid cpt ->
                  Printf.printf "      * %d warnings(s) raised by %S/%S/%i\n%!"
                    cpt pname lname wid)
                wtbl)
          ptbl)
    breakdown_linted;
  Printf.printf "  * errors(s) on %d file(s) were emitted:\n%!"
    files_linted_errors_total;
  StringCompat.StringSet.iter
    (Printf.printf "    - %s\n%!") !files_linted_errors

let print fmt severity path db =
  try
    Hashtbl.iter (fun file (hash, pres) ->
        if Lint_utils.(is_in_path !Lint_db.DefaultDB.root file path) then
          StringMap.iter (fun pname lres ->
              let flag = check_flag [pname; "enabled"] in
              if flag then
                StringMap.iter  (fun lname (_source, _opt, ws) ->
                    let flag = check_flag [pname; lname; "enabled"] in
                    if flag then
                      let filters =
                        Lint_globals.Config.get_option_value
                          [pname; lname; "warnings"] in
                      let arr = Lint_parse_args.parse_options filters in
                      List.iter
                        (fun warning ->
                           if arr.(warning.decl.id - 1) &&
                              warning.decl.severity >= severity then
                             print_warning fmt pname lname warning)
                        ws)
                  lres)
            pres)
      db
  with Not_found ->
    Printf.eprintf "Warning: Database contains warnings raised by plugins \
                    that do not exist anymore. Please clean your database.\n%!"

let print_error ppf path db_error =
  let has_error = ref false in
  Hashtbl.iter (fun file error_set ->
      if Lint_utils.(is_in_path !Lint_db.DefaultDB.root file path) &&
         not (ErrorSet.is_empty error_set) then
        has_error := true
    ) db_error;
  if !has_error then
    begin
      Format.fprintf ppf "=== Errors ===\n%!";
      Hashtbl.iter (fun file error_set ->
          if Lint_utils.(is_in_path !Lint_db.DefaultDB.root file path) &&
             not (ErrorSet.is_empty error_set) then
            begin
              Format.fprintf ppf "%S:\n%!" file;
              ErrorSet.iter (print_error ppf) error_set
            end)
        db_error;
      Format.fprintf ppf "==============\n%!"
    end

let print_only_new fmt path db =
  Hashtbl.iter (fun file (hash, pres) ->
      if Lint_utils.(is_in_path !Lint_db.DefaultDB.root file path) then
        StringMap.iter (fun pname lres ->
            StringMap.iter  (fun lname (source, _opt, ws) ->
                if source = Analyse then
                  List.iter (print_warning fmt pname lname) ws)
              lres)
          pres)
    db

let verbose_info fmt db =
  Hashtbl.iter (fun file (hash, pres) ->
      let triggered_plugin =
        StringMap.filter (fun pname lres ->
            StringMap.exists (fun lname (source, opt, ws) ->
                let plugin_flag = check_flag [pname; "enabled"] in
                let linter_flag = check_flag [pname; lname; "enabled"] in
                let filters =
                  Lint_globals.Config.get_option_value
                    [pname; lname; "warnings"] in
                let arr = Lint_parse_args.parse_options filters in
                let warnings_activated =
                  List.filter (fun warning ->
                      arr.(warning.decl.id - 1)) ws in
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
