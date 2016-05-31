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

let print_warning ppf linter_name warning =
  if warning.loc <> Location.none then
    Format.fprintf ppf "%a" Location.print warning.loc;

  Format.fprintf ppf "  Warning %S number %d:\n"
    linter_name warning.instance.id;
  Format.fprintf ppf "  %s" warning.output;
  Format.fprintf ppf "@."

let check_flag options =
  try
    bool_of_string
      (Lint_globals.Config.get_option_value options)
  with Not_found -> true

let summary path db =
  let files_linted = ref StringCompat.StringSet.empty in
  let breakdown = Hashtbl.create 42 in
  Hashtbl.iter (fun file (hash, pres) ->
      if Lint_utils.(is_in_path file (absolute path)) then
        StringMap.iter (fun pname lres ->
            let flag = check_flag [pname; "flag"] in
            if flag then
              StringMap.iter  (fun lname (source, _opt, ws) ->
                  let flag = check_flag [pname; lname; "flag"] in
                  if flag && source = Analyse then
                    let filters =
                      Lint_globals.Config.get_option_value
                        [pname; lname; "warnings"] in
                    let arr = Lint_parse_args.parse_options filters in
                    List.iter
                      (fun warning ->
                         if arr.(warning.instance.id - 1) then
                           (files_linted :=
                              StringCompat.StringSet.add file !files_linted;
                            let entry = lname, warning.instance.id in
                            if Hashtbl.mem breakdown entry then
                              let old_cpt = Hashtbl.find breakdown entry in
                              Hashtbl.replace breakdown entry (old_cpt + 1)
                            else Hashtbl.add breakdown entry 1)
                      )
                      ws)
                lres)
          pres)
    db;
  let files = StringCompat.StringSet.cardinal !files_linted in
  let total = Hashtbl.fold (fun (_, _) cpt acc -> acc + cpt) breakdown 0 in
  Printf.printf "Summary:\n%!";
  Printf.printf "* %d files were linted\n%!" files;
  Printf.printf "* %d warnings were emitted:\n%!" total;
  Hashtbl.iter (fun (lname, id) cpt ->
      Printf.printf "  * %d %S number %d\n%!" cpt lname id)
    breakdown

let print fmt path db =
  try
    Hashtbl.iter (fun file (hash, pres) ->
        if Lint_utils.(is_in_path file (absolute path)) then
          StringMap.iter (fun pname lres ->
              let flag = check_flag [pname; "flag"] in
              if flag then
                StringMap.iter  (fun lname (_source, _opt, ws) ->
                    let flag = check_flag [pname; lname; "flag"] in
                    if flag then
                      let filters =
                        Lint_globals.Config.get_option_value
                          [pname; lname; "warnings"] in
                      let arr = Lint_parse_args.parse_options filters in
                      List.iter
                        (fun warning ->
                           if arr.(warning.instance.id - 1) then
                             print_warning fmt lname warning)
                        ws)
                  lres)
            pres)
      db
  with Not_found ->
    Printf.eprintf "Warning: Database contains warnings raised by plugins \
                    that do not exist anymore. Please clean your database.\n%!"

let print_only_new fmt path db =
  Hashtbl.iter (fun file (hash, pres) ->
      if Lint_utils.(is_in_path file (absolute path)) then
        StringMap.iter (fun pname lres ->
            StringMap.iter  (fun lname (source, _opt, ws) ->
                if source = Analyse then List.iter (print_warning fmt lname) ws)
              lres)
          pres)
    db
