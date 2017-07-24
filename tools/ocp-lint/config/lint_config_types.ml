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
  val config_file_name : string

  val config_file : SimpleConfig.config_file

  val init_config : FileGen.t -> unit

  val simple_args : unit -> (string * Arg.spec * string) list

  val create_option :
    names:string list ->
    shelp:string ->
    lhelp:string ->
    level:int ->
    ty:'a SimpleConfig.option_class ->
    default:'a ->
    'a SimpleConfig.config_option

  val get_option_value : string list -> string

  val get_linter_options_details :
    pname:string ->
    lname:string -> (string * string * string) list

  val get_linter_options :
    pname:string ->
    lname:string -> (string * string) list

  val save : unit -> unit

  val save_master : string -> unit

  val load_and_save : string -> string list -> string

  val load_config_tmp : master:string -> config:string -> unit

end
