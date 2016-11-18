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

  (* [simple_args ()] produce Arg arguments from the config file *)
  val simple_args : unit -> (string * Arg.spec * string) list

  (* [create_option names shelp lhelp level ty default] create_option [names]
     of type [ty] with the default value [default], a short help [shelp],
     a long help [lhelp] and a [level] *)
  val create_option :
    names:string list ->
    shelp:string ->
    lhelp:string ->
    level:int ->
    ty:'a SimpleConfig.option_class ->
    default:'a ->
    'a SimpleConfig.config_option

  (* [get_option_value names] get the value of the option [names].*)
  val get_option_value : string list -> string

  (* [get_linter_options pname lname] get all options with its help and
     default value for the linter  pname:lname. *)
  val get_linter_options_details :
    pname:string ->
    lname:string -> (string list * string list* string) list

  (* [get_linter_options pname lname] get all options for the linter
     pname:lname. *)
  val get_linter_options :
    pname:string ->
    lname:string -> (string list * string) list

  (* [save ()] save the current options value in the config file. *)
  val save : unit -> unit

  (* [save_master filename] save the config under [filename]
     without changing the file associated with the config. *)
  val save_master : string -> unit

  (* [load_and_save master configs] load [master], load .ocplint
     from [configs], returned a temporary  saved config. *)
   val load_and_save : string -> string list -> string

  (* [load_config_tmp master config] load [master] and then [config]. *)
  val load_config_tmp : master:string -> config:string -> unit

end
