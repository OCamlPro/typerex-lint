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

open Lint_db_types

let empty_db () = Hashtbl.create 42

let string_of_source = function
  | Cache -> "Cache"
  | Analyse -> "Analyse"

module MakeDB (DB : DATABASE_IO) = struct
  let version = 1

  let root = ref ""

  let db = empty_db ()
  let db_errors = empty_db ()

  let db_hash file =
    let file_content = Lint_utils.read_file file in
    let string_to_hash = file ^ file_content in
    Digest.string string_to_hash

  let load dir =
    let db = empty_db () in
    if Sys.is_directory dir then
      let files = Sys.readdir dir in
      Array.iter (fun file ->
        let path = Filename.concat dir file in
        try
          let (version_db, date, filename, pres, file_error) =
            DB.load path in
          (Hashtbl.add db filename (file, pres);
           Hashtbl.add db_errors file file_error)
        with exn ->
          Format.eprintf "Can't read DB file for %S, skipping it\n%!" file)
        files;
      db
    else
      (Printf.eprintf "Db Error : %s should be a dir\n%!" dir;
       db)

  let load_file file =
    let hash = db_hash file in
    let hash_filename = Digest.to_hex hash in
    let olint_dir = Filename.concat !root "_olint" in
    let hash_filename_path = Filename.concat olint_dir hash_filename in
    if Sys.file_exists hash_filename_path then
      try
        let (version_db, date, file, pres, file_error) =
          DB.load hash_filename_path in
        (Hashtbl.add db file (hash, pres);
         Hashtbl.add db_errors file file_error)
      with exn ->
        Format.eprintf "Can't read DB file for %S, skipping it\n%!" file

  let init path =
    let path = File.to_string path in
    root := Filename.dirname path

  let cache ()=
    Hashtbl.iter (fun file (hash, pres) ->
        let new_pres =
          StringMap.fold (fun pname lres acc ->
              let new_lres =
                StringMap.fold  (fun lname res acc ->
                    StringMap.add lname { res with res_source = Cache } acc)
                  lres StringMap.empty in
              StringMap.add pname new_lres acc)
            pres StringMap.empty in
        Hashtbl.replace db file (hash, new_pres))
      db

  let save () =
    Hashtbl.iter (fun file (hash, pres) ->
        let db_path = Filename.concat !root "_olint" in
        let db_file = Filename.concat db_path (Digest.to_hex hash) in
        let db_file_tmp = db_file ^ ".tmp" in
        let file_error =
          if Hashtbl.mem db_errors file then
            Hashtbl.find db_errors file
          else ErrorSet.empty in
        let date = Unix.time () in
        DB.save db_file_tmp (version, date, file, pres, file_error);
        Sys.rename db_file_tmp db_file)
      db

  let reset () = Hashtbl.reset db

  let merge sources = List.iter load_file sources

  let print_debug db =
    Printf.printf "============= DB Debug ============\n%!";
    Hashtbl.iter (fun file (hash, pres) ->
        Printf.printf "%s[HASH] :\n%!" file;
        StringMap.iter (fun pname lres ->
            Printf.printf "  %s :\n%!" pname;
            StringMap.iter  (fun lname { res_warnings; res_source } ->
                Printf.printf "    %s-%s[%d] - | %!"
                  lname (string_of_source res_source)
                  (List.length res_warnings);
                List.iter (fun w ->
                    Printf.printf "%s; " w.Lint_warning_types.output)
                  res_warnings;
                Printf.printf " |\n%!")
              lres)
          pres)
      db;
    Printf.printf "=========================\n%!"

  let remove_entry file =
    let file = Lint_utils.normalize_path !root file in
    Hashtbl.remove db file

  let add_entry file pname lname res_version =
    let res_source = Analyse in
    let res_options =
      Lint_config.DefaultConfig.get_linter_options pname lname in
    let hash = db_hash file in
    let file = Lint_utils.normalize_path !root file in
    match Hashtbl.find db file with
    | exception Not_found ->
      let new_pres = StringMap.add
          lname { res_version; res_source; res_options;
                  res_warnings = [] } StringMap.empty in
      let new_fres = StringMap.add pname new_pres StringMap.empty in
      Hashtbl.add db file (hash, new_fres)
    | (_old_hash, old_fres) ->
      match StringMap.find pname old_fres with
      | exception Not_found ->
        let new_pres = StringMap.add
            lname { res_version; res_source; res_options;
                    res_warnings = [] } StringMap.empty in
        let new_fres = StringMap.add pname new_pres old_fres in
        Hashtbl.replace db file (hash, new_fres)

      | old_pres ->
        (* if linter already register, nothing to do *)
        (* this can happen when a linter as several mains *)
        if not (StringMap.mem lname old_pres) then
          let new_pres =
            StringMap.add
              lname { res_version; res_source; res_options;
                      res_warnings =  [] } old_pres in
          let new_fres = StringMap.add
              pname new_pres (StringMap.remove pname old_fres) in
          Hashtbl.replace db file (hash, new_fres)
        else ()

  let add_error file error =
    let file = Lint_utils.normalize_path !root file in
    if Hashtbl.mem db_errors file then
      let error_set = Hashtbl.find db_errors file in
      Hashtbl.replace db_errors file (ErrorSet.add error error_set)
    else
      Hashtbl.add db_errors file (ErrorSet.singleton error)

  let clean limit_time =
    let db_path = Filename.concat !root "_olint" in
    let files = Sys.readdir db_path in
    let curr_date = Unix.time () in
    Array.iter (fun file ->
        let file_path = Filename.concat db_path file in
        let (version_db, date, file, pres, file_error) =
          DB.load file_path in
        let limit_date = date +. (86400. *. (float limit_time)) in
        if version <> version_db then Sys.remove file_path;
        if curr_date >= limit_date then Sys.remove file_path)
      files

  let update pname lname warn =
    let file =
      warn.Lint_warning_types.loc.Location.loc_start.Lexing.pos_fname in
    if not (Sys.file_exists file)
    then raise (Lint_db_error.Db_error (Lint_db_error.File_not_found file));
    let hash = db_hash file in
    let file = Lint_utils.normalize_path !root file in
    if Hashtbl.mem db file then
      let (_, old_pres) = Hashtbl.find db file in
      if StringMap.mem pname old_pres then
        let old_lres = StringMap.find pname old_pres in
        if StringMap.mem lname old_lres then
          let res (* version, src, opt, old_wres *) =
            StringMap.find lname old_lres in
          let new_wres = { res with
                           res_warnings =  warn :: res.res_warnings } in
          let new_lres =
            StringMap.add lname new_wres (StringMap.remove lname old_lres) in
          let new_pres =
            StringMap.add pname new_lres (StringMap.remove pname old_pres) in
          Hashtbl.replace db file (hash, new_pres)
        else
          raise (Lint_db_error.Db_error
                   (Lint_db_error.Linter_not_in_db (file, pname, lname)))
      else
        raise (Lint_db_error.Db_error
                 (Lint_db_error.Plugin_not_in_db (file, pname)))
    else
      raise (Lint_db_error.Db_error (Lint_db_error.File_not_in_db file))

  let already_run file pname lname version =
    let new_hash = db_hash file in
    let file = Lint_utils.normalize_path !root file in
    if Hashtbl.mem db file then
      let (hash, old_fres) = Hashtbl.find db file in
      if hash = new_hash && StringMap.mem pname old_fres then
        let old_pres = StringMap.find pname old_fres in
        if StringMap.mem lname old_pres then
          let { res_version;
                res_options } = StringMap.find lname old_pres in
          let options =
            Lint_config.DefaultConfig.get_linter_options pname lname in
          options = res_options && version = res_version
        else false
      else false
    else false

  let has_warning () =
    let warning_count =
      Hashtbl.fold (fun file (hash, pres) count ->
          StringMap.fold (fun pname lres count ->
              StringMap.fold  (fun lname { res_warnings } count ->
                  count + (List.length res_warnings))
                lres count)
            pres count)
        db 0 in
    warning_count > 0

end

module Marshal_IO : DATABASE_IO = struct

  let load file =
    let ic = open_in file in
    let db = input_value ic in
    close_in ic;
    db

  let save file db =
    let oc = open_out file in
    output_value oc db;
    close_out oc

end

module DefaultDB : DATABASE = MakeDB (Marshal_IO)
