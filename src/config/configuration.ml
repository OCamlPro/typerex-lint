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

module type ConfigArg = sig
  val filename : string
end

module type CONFIG = sig
  val config_file : SimpleConfig.config_file
  val simple_args : unit -> (string * Arg.spec * string) list
  val create_option :
    string list ->
    ?short_help:string ->
    string list ->
    ?level:int ->
    'a SimpleConfig.option_class ->
    'a ->
    'a SimpleConfig.config_option
end

module MakeConfig (C: ConfigArg) = struct
  let config_file = SimpleConfig.create_config_file (File.of_string C.filename)

  let simple_args () = []
    (* Uncomment this when ocp-build will be updated *)
    (* SimpleConfig.LowLevel.simple_args "" config_file *)

  let create_option
      opt_names ?short_help long_help ?level opt_class default_value =
      SimpleConfig.create_option config_file
          opt_names ?short_help long_help ?level
          opt_class default_value
  let () =
    SimpleConfig.load config_file
  end

module DefaultConfig : CONFIG = MakeConfig(struct let filename = ".ocplint" end)
