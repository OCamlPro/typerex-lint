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

module type CONFIG = sig
  val config_file : SimpleConfig.config_file
  val simple_args : unit -> (string * Arg.spec * string) list
  val create_option :
    string list ->
    string ->
    string ->
    int ->
    'a SimpleConfig.option_class ->
    'a ->
    'a SimpleConfig.config_option
  val get_option_value : string list -> string
  val get_linter_options : string -> string -> (string list * string) list
  val save : unit -> unit
end

exception ConfigParseError of string

module DefaultConfig = struct
  let config_file = SimpleConfig.create_config_file (File.of_string ".ocplint")

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

  let string_of_string_list list =
    let rec iter s list =
      match list with
        [] -> s
      | ss :: tail ->
        iter (Printf.sprintf "%s.%s" s ss) tail
    in
    match list with
      [] -> ""
    | s :: tail -> iter s tail

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

  let () =
    try
      SimpleConfig.load config_file
    with Failure msg -> raise (ConfigParseError msg)

  end

