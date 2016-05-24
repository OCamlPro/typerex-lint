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

module StringMap = Map.Make (String)

type source = Cache | Analyse

type warning_list =
  source * (string list * string) list * Warning_types.warning list
type linter_map = warning_list StringMap.t
type plugin_map = linter_map StringMap.t
type file_map = Digest.t * plugin_map
type t = (string, file_map) Hashtbl.t

let empty_db () = Hashtbl.create 42

let string_of_source = function
  | Cache -> "Cache"
  | Analyse -> "Analyse"

module type DATABASE_IO = sig
  val load : string -> t
  val save : string -> t -> unit
end

module type DATABASE = sig
  val init : File.t -> unit
  val save : unit -> unit
  val reset : unit -> unit
  val print : Format.formatter -> unit
  val print_only_new : Format.formatter -> unit
  val print_debug : unit -> unit
  val remove_entry : string -> unit
  val add_entry : string -> string -> string -> unit
  val clean : string list -> unit
  val update : string -> string -> Warning_types.warning -> unit
  val already_run : string -> string -> string -> bool
  val has_warning : unit -> bool
end

module MakeDB (DB : DATABASE_IO) = struct
  let database_file = ref ""

  let db = empty_db ()

  let init path =
    try
      let file = Filename.concat (File.to_string path) "db" in
      database_file := file;
      let db2 = DB.load !database_file in
      Hashtbl.iter (fun k v -> Hashtbl.add db k v) db2
    with Not_found ->
      Printf.eprintf
         "%s, %s"
         "Can't find .typerex-lint dir"
         "use ocplint init if you want to use db support.\n%!"

  let save () =
    Hashtbl.iter (fun file (hash, pres) ->
        let new_pres =
          StringMap.fold (fun pname lres acc ->
              let new_lres =
                StringMap.fold  (fun lname (_src, opt, ws) acc ->
                    StringMap.add lname (Cache, opt, ws) acc)
                  lres StringMap.empty in
              StringMap.add pname new_lres acc)
            pres StringMap.empty in
        Hashtbl.replace db file (hash, new_pres))
      db;
    DB.save !database_file db

  let reset () = Hashtbl.reset db

  let print_debug () =
    Printf.printf "============= DB Debug ============\n%!";
    Hashtbl.iter (fun file (hash, pres) ->
        Printf.printf "%s[HASH] :\n%!" file;
        StringMap.iter (fun pname lres ->
            Printf.printf "  %s :\n%!" pname;
            StringMap.iter  (fun lname (source, _opt, ws) ->
                Printf.printf "    %s-%s[%d] - | %!"
                  lname (string_of_source source) (List.length ws);
                List.iter (fun w ->
                    Printf.printf "%s; " w.Warning_types.output) ws;
                Printf.printf " |\n%!")
              lres)
          pres)
      db;
    Printf.printf "=========================\n%!"

  let print fmt =
    Hashtbl.iter (fun file (hash, pres) ->
        StringMap.iter (fun pname lres ->
            StringMap.iter  (fun lname (_source, _opt, ws) ->
                List.iter (Text.print fmt) ws)
              lres)
          pres)
      db

  let print_only_new fmt =
    Hashtbl.iter (fun file (hash, pres) ->
        StringMap.iter (fun pname lres ->
            StringMap.iter  (fun lname (source, _opt, ws) ->
                if source = Analyse then List.iter (Text.print fmt) ws)
              lres)
          pres)
      db

  let remove_entry file =
    let file = Utils.absolute file in
    Hashtbl.remove db file

  let add_entry file pname lname =
    let options = Configuration.DefaultConfig.get_linter_options pname lname in
    let file = Utils.absolute file in
    let hash = Digest.file file in
    if Hashtbl.mem db file then
      let (_old_hash, old_fres) = Hashtbl.find db file in
      if StringMap.mem pname old_fres then
        let old_pres = StringMap.find pname old_fres in
        let new_pres =
          StringMap.add
            lname (Analyse, options, []) (StringMap.remove lname old_pres) in
        let new_fres = StringMap.add
            pname new_pres (StringMap.remove pname old_fres) in
        Hashtbl.replace db file (hash, new_fres)
      else
        let new_pres = StringMap.add
            lname (Analyse, options, []) StringMap.empty in
        let new_fres = StringMap.add pname new_pres old_fres in
        Hashtbl.replace db file (hash, new_fres)
    else
      let new_pres = StringMap.add
          lname (Analyse, options, []) StringMap.empty in
      let new_fres = StringMap.add pname new_pres StringMap.empty in
      Hashtbl.add db file (hash, new_fres)

  let clean files =
    List.iter (fun file ->
        let file = Utils.absolute file in
        if Hashtbl.mem db file then
          let hash = Digest.file file in
          let (old_hash, _old_fres) = Hashtbl.find db file in
          if old_hash = hash then ()
          else remove_entry file)
      files

  let update pname lname warn =
    let file = warn.Warning_types.loc.Location.loc_start.Lexing.pos_fname in
    if not (Sys.file_exists file)
    then raise (Db_error.Db_error (Db_error.File_not_found file));
    let file = Utils.absolute file in
    let hash = Digest.file file in
    if Hashtbl.mem db file then
      let (_, old_pres) = Hashtbl.find db file in
      if StringMap.mem pname old_pres then
        let old_lres = StringMap.find pname old_pres in
        if StringMap.mem lname old_lres then
          let src, opt, old_wres = StringMap.find lname old_lres in
          let new_wres = src, opt, warn :: old_wres in
          let new_lres =
            StringMap.add lname new_wres (StringMap.remove lname old_lres) in
          let new_pres =
            StringMap.add pname new_lres (StringMap.remove pname old_pres) in
          Hashtbl.replace db file (hash, new_pres)
        else raise (Db_error.Db_error (Db_error.Linter_not_in_db (file, pname, lname)))
      else raise (Db_error.Db_error (Db_error.Plugin_not_in_db (file, pname)))
    else raise (Db_error.Db_error (Db_error.File_not_in_db file))

  let already_run file pname lname =
    let file = Utils.absolute file in
    let new_hash = Digest.file file in
    if Hashtbl.mem db file then
      let (hash, old_fres) = Hashtbl.find db file in
      if hash = new_hash then
        if StringMap.mem pname old_fres then
          let old_pres = StringMap.find pname old_fres in
          if StringMap.mem lname old_pres then
            let _src, opt, _warn = StringMap.find lname old_pres in
            let options =
              Configuration.DefaultConfig.get_linter_options pname lname in
            options = opt
          else false
        else false
      else false
    else false

  let has_warning () =
    let warning_count =
      Hashtbl.fold (fun file (hash, pres) count ->
          StringMap.fold (fun pname lres count ->
              StringMap.fold  (fun lname (_src, _opt, ws) count ->
                  count + (List.length ws))
                lres count)
            pres count)
        db 0 in
    warning_count > 0

end

module Marshal_io : DATABASE_IO = struct

  let load file =
    if Sys.file_exists file then
      try
        let ic = open_in file in
        input_value ic
      with _ ->
        (Printf.eprintf "Can't read DB file, using a fresh DB\n%!";
         empty_db ())
    else
      (Printf.eprintf "No DB file found, using a fresh DB\n%!";
       empty_db ())

  let save file db =
    let oc = open_out file in
    output_value oc db

end

module DefaultDB : DATABASE = MakeDB (Marshal_io)
