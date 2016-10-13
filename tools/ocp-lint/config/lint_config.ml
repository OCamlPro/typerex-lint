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

  let dot_file = ref ""

  let config_file = SimpleConfig.create_config_file (File.of_string "")

  let init_config set_dot_file file =
    dot_file := set_dot_file;
    SimpleConfig.set_config_file config_file file;
    SimpleConfig.load config_file

  let simple_args () =
    SimpleConfig.LowLevel.simple_args "" config_file

  let create_option
      opt_names short_help long_help level opt_class default_value =
    let short_help = Some short_help in
    let level = Some level in
    SimpleConfig.create_option config_file
      opt_names ?short_help:short_help [long_help] ?level:level
      opt_class default_value

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

  let get_linter_options plugin_name linter_name =
    let name = [ plugin_name; linter_name ] in
    let options = SimpleConfig.LowLevel.simple_options "" config_file in
    let plugin_options = List.filter (fun oi -> is_option_of name oi) options in
    List.map (fun oi ->
        oi.SimpleConfig.LowLevel.option_name,
        oi.SimpleConfig.LowLevel.option_value)
      plugin_options

  let save () =
    SimpleConfig.save_with_help config_file

  (* Save the config under [filename] without changing the file
     associated with the config. *)
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
        let cfg = Filename.concat cfg_dir !dot_file in
        let cfg = File.of_string cfg in
        SimpleConfig.set_config_file config_file cfg;
        SimpleConfig.load config_file)
      configs

  (* load [master], load .ocplint from [configs], returned a temporary
     saved config. *)
  let load_and_save master configs =
    let master_t = File.of_string master in
    let filename = Filename.temp_file !dot_file "" in
    let filename_t = File.of_string filename in
    load_configs master_t configs;
    SimpleConfig.set_config_file config_file filename_t;
    SimpleConfig.save config_file;
    SimpleConfig.set_config_file config_file master_t;
    SimpleConfig.load config_file;
    File.to_string filename_t

  (* load [master] and then [config] *)
  let load_config_tmp master config =
    let master = File.of_string master in
    let config = File.of_string config in
    SimpleConfig.set_config_file config_file master;
    SimpleConfig.load config_file;
    SimpleConfig.set_config_file config_file config;
    SimpleConfig.load config_file

end
