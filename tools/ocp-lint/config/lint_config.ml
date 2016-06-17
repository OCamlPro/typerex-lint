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
  let config_file = SimpleConfig.create_config_file (File.of_string ".ocplint")

  let init_config file =
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

  let is_option_of name oi =
    try
      let diff =
        List.mapi (fun i n ->
            (List.nth oi.SimpleConfig.LowLevel.option_name i) = n)
          name in
      List.for_all (fun b -> b) diff
    with _ -> false

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
end
