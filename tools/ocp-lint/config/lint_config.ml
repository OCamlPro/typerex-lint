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

open Lint_config_types

exception ConfigParseError of string

module DefaultConfig = struct

  let config_file_name = ".ocplint"

  let config_file = SimpleConfig.create_config_file (File.of_string "")

  let init_config file =
    SimpleConfig.set_config_file config_file file;
    SimpleConfig.load config_file

  let simple_args () =
    SimpleConfig.LowLevel.simple_args "" config_file

  let create_option
      ~names ~shelp ~lhelp ~level ~ty ~default =
    let short_help = Some shelp in
    let level = Some level in
    let lhelp = if lhelp = "" then [] else [ lhelp ] in
    SimpleConfig.create_option
      config_file
      names
      ?short_help:short_help
      lhelp
      ?level:level
      ty
      default

  let get_option_value option_name =
    SimpleConfig.LowLevel.get_simple_option config_file option_name

  (* Move to OcpList or even to List *)
  let rec list_starts_with oi name =
    match oi, name with
    | _, [] -> true
    | [], _ -> false
    | x :: oi, y :: name when x = y -> list_starts_with oi name
    | _ -> false

  let is_option_of name oi =
    list_starts_with oi.SimpleConfig.LowLevel.option_name name

  let get_linter_options_details ~pname ~lname =
    let open SimpleConfig.LowLevel in
    let opts = ref [] in
    List.iter (fun sec ->
        iter_section (fun opt ->
            let name = shortname opt in
            let prefix = Printf.sprintf "%s.%s" pname lname in
            let re = Str.regexp_string prefix in
            let found =
              try (Str.search_forward re (shortname opt) 0) = 0
              with Not_found -> false in
            if found then begin
              let help = get_help opt in
              let o = SimpleConfig.(!!) opt in
              let cl = get_class opt in
              let value = to_value cl o in
              let str_value =
                try value_to_string value
                with Failure _ ->
                  begin
                    match value with
                    | DelayedValue f ->
                      let buf = Buffer.create 64 in
                      f buf "";
                      Str.global_replace
                        (Str.regexp_string "\n")
                        ""
                        (Buffer.contents buf)
                    | _ -> ""
                  end in
              opts := (name, help, str_value)::!opts
            end)
          sec)
      (sections config_file);
    !opts

  let get_linter_options ~pname ~lname =
    let open SimpleConfig.LowLevel in
    let opts = ref [] in
    List.iter (fun sec ->
        iter_section (fun opt ->
            let name = shortname opt in
            let prefix = Printf.sprintf "%s.%s" pname lname in
            let re = Str.regexp_string prefix in
            let found =
              try (Str.search_forward re (shortname opt) 0) = 0
              with Not_found -> false in
            let ignore_opt =  Printf.sprintf "%s.%s.ignore" pname lname in
            if found && name <> ignore_opt then begin
              let o = SimpleConfig.(!!) opt in
              let cl = get_class opt in
              let value = to_value cl o in
              (* We marshal with Closures flag the values of options so complex
                 options can be stored in db *)
              let str_value = Marshal.to_string value [ Marshal.Closures ] in
              opts := (name, str_value)::!opts
            end)
          sec)
      (sections config_file);
    !opts

  let save () =
    SimpleConfig.set_config_file config_file (File.of_string config_file_name);
    SimpleConfig.save_with_help config_file

  let save_master filename =
    let filename = File.of_string filename in
    let old_filename = SimpleConfig.config_file config_file in
    SimpleConfig.set_config_file config_file filename;
    SimpleConfig.save config_file;
    SimpleConfig.set_config_file config_file old_filename

  (* Load [master] file, then load ".ocplint" present in each
     directory of [configs]. *)
  let load_configs master configs =
    SimpleConfig.set_config_file config_file master;
    SimpleConfig.load config_file;
    List.iter (fun cfg_dir ->
        let cfg = Filename.concat cfg_dir config_file_name in
        let cfg = File.of_string cfg in
        SimpleConfig.set_config_file config_file cfg;
        SimpleConfig.load config_file)
      configs

  let load_and_save master configs =
    let master_t = File.of_string master in
    let filename = Filename.temp_file config_file_name "" in
    let filename_t = File.of_string filename in
    load_configs master_t configs;
    SimpleConfig.set_config_file config_file filename_t;
    SimpleConfig.save config_file;
    SimpleConfig.set_config_file config_file master_t;
    SimpleConfig.load config_file;
    File.to_string filename_t

  let load_config_tmp ~master ~config =
    let master = File.of_string master in
    let config = File.of_string config in
    SimpleConfig.set_config_file config_file master;
    SimpleConfig.load config_file;
    SimpleConfig.set_config_file config_file config;
    SimpleConfig.load config_file

end
