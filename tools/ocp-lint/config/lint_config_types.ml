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

  (* [init_config dot_file filename] reads the initial configuration *)
  val init_config : string -> File.t -> unit

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

  val save_master : string -> unit

  val load_and_save : string -> string list -> string

  val load_config_tmp : string -> string -> unit

end
