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

  Format.fprintf ppf "  Warning %d: %s/%s/%s\n"
    warning.decl.id pname lname warning.decl.short_name;
  Format.fprintf ppf "  %s" warning.output;
  Format.fprintf ppf "@."

let print_error ppf error =
  let str = match error with
    | Db_error err -> Printf.eprintf "Db_error\n%!"; Lint_db_error.to_string err
    | Plugin_error err -> Printf.eprintf "Plugin_error\n%!"; Lint_plugin_error.to_string err
    | Sempatch_error err -> Printf.eprintf "Sem_error\n%!"; Sempatch.Failure.to_string err
    | Ocplint_error str -> Printf.eprintf "OCPL_error\n%!"; str
  in
  Format.fprintf ppf "  %s\n%!" str

let check_flag options =
  try
    bool_of_string
      (Lint_globals.Config.get_option_value options)
  with Not_found -> true

let summary path db db_errors =
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
                           if arr.(warning.decl.id - 1) then
                             (files_linted :=
                                StringCompat.StringSet.add file !files_linted;
                              let entry = lname, warning.decl.id in
                              if Hashtbl.mem breakdown_linted entry then
                                let old_cpt = Hashtbl.find breakdown_linted entry in
                                Hashtbl.replace breakdown_linted entry (old_cpt + 1)
                              else Hashtbl.add breakdown_linted entry 1))
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
                           if arr.(warning.decl.id - 1) then
                             (files_cached :=
                                StringCompat.StringSet.add file !files_cached;
                              let entry = lname, warning.decl.id in
                              if Hashtbl.mem breakdown_cached entry then
                                let old_cpt = Hashtbl.find breakdown_cached entry in
                                Hashtbl.replace breakdown_cached entry (old_cpt + 1)
                              else Hashtbl.add breakdown_cached entry 1))
                      ws end)
                lres)
          pres)
    db;
  let files_cached_total = StringCompat.StringSet.cardinal !files_cached in
  let warnings_cached_total =
    Hashtbl.fold (fun (_, _) cpt acc -> acc + cpt) breakdown_cached 0 in
  let files_linted_total = StringCompat.StringSet.cardinal !files_linted in
  let warnings_linted_total =
    Hashtbl.fold (fun (_, _) cpt acc -> acc + cpt) breakdown_linted 0 in
  let files_linted_errors_total = StringCompat.StringSet.cardinal !files_linted_errors in
  let files_cached_errors_total = StringCompat.StringSet.cardinal !files_cached_errors in
  Printf.printf "Summary:\n%!";
  Printf.printf "== Infos ==\n%!";
  Printf.printf "  * root dir: %s\n%!" !(Lint_db.DefaultDB.root);
  Printf.printf "== Cache Infos ==\n%!";
  Printf.printf "  * %d file(s) were found in cache:\n%!" files_cached_total;
  StringCompat.StringSet.iter (Printf.printf "    - %s\n%!") !files_cached;
  Printf.printf "  * %d warning(s) were found in cache:\n%!" warnings_cached_total;
  Hashtbl.iter (fun (lname, id) cpt ->
      Printf.printf "    - %d %S number %d\n%!" cpt lname id)
    breakdown_cached;
  Printf.printf "  * error(s) on %d file(s) were found in cache:\n%!" files_cached_errors_total;
  StringCompat.StringSet.iter (Printf.printf "    - %s\n%!") !files_cached_errors;
  Printf.printf "== New Warnings ==\n%!";
  Printf.printf "  * %d file(s) were linted:\n%!" files_linted_total;
  StringCompat.StringSet.iter (Printf.printf "    - %s\n%!") !files_linted;
  Printf.printf "  * %d warning(s) were emitted:\n%!" warnings_linted_total;
  Hashtbl.iter (fun (lname, id) cpt ->
      Printf.printf "    - %d %S number %d\n%!" cpt lname id)
    breakdown_linted;
  Printf.printf "  * errors(s) on %d file(s) were emitted:\n%!" files_linted_errors_total;
  StringCompat.StringSet.iter (Printf.printf "    - %s\n%!") !files_linted_errors

let print fmt path db =
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
                           if arr.(warning.decl.id - 1) then
                             print_warning fmt pname lname warning)
                        ws)
                  lres)
            pres)
      db
  with Not_found ->
    Printf.eprintf "Warning: Database contains warnings raised by plugins \
                    that do not exist anymore. Please clean your database.\n%!"

let print_error ppf path db_error =
  Format.fprintf ppf "=== Errors ===\n%!";
  Hashtbl.iter (fun file error_set ->
      if Lint_utils.(is_in_path !Lint_db.DefaultDB.root file path) && not (ErrorSet.is_empty error_set) then
        begin
          Format.fprintf ppf "%S:\n%!" file;
          ErrorSet.iter (print_error ppf) error_set
        end)
    db_error;
  Format.fprintf ppf "==============\n%!"

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
