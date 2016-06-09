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

(** [MakePlugin] is a functor which take a module of type
    [Lint_plugin_types.PluginArg] as an argument. It registers the plugin to the
    global [Globals.plugins] and all the linter associated and created with
    the functor [MakeLint]. *)
module MakePlugin : functor (Plugin : Lint_plugin_types.PLUGINARG) ->
sig

  (** The name of the plugin.  *)
  val name : string
  (** The short name of the plugin. It must be unique for each plugin. *)
  val short_name : string
  (** The details of the plugin.  *)
  val details : string

  (** [MakeLintPatch] is a functor which takes a module of type
      [Lint.LintPatchArg] as argument. It allows to create a linter based
      on semantic patches (see ocplib-sempatch). It takes the files names of
      the patches as arguments and automatically register the linter
      into a plugin. *)
  module MakeLintPatch : functor (CA : Lint_types.LINTPATCHARG) ->  sig end

  (** [MakeLint] is a functor which takes a module of type [Lint.LintArg] as
      argument. It allows to create a linter and automatically register it to
      the plugin in the global [Globals.plugins]. The functor allows to create
      specific options for each lint and register them to the global
      [Global.Config] module. *)
  module MakeLint :
    functor (CA : Lint_types.LINTARG) ->
    sig
      (** The name of the linter. *)
      val name : string
      (** The short name of the linter. It must be unique for each linter. *)
      val short_name : string
      (** The details message of the linter. *)
      val details : string

      (** [new_warning loc id kinds ~short_name ~msg ~args] registers a warning
          with at the location [loc], the warning number [id], a short
          description [short_name], the message [msg] which will be displayed
          and the [args] which is a list of couple of string which will be
          substitute in the displayed message. *)
      val new_warning :
        id:int ->
        short_name:string ->
        msg:string ->
        Lint_warning_types.warning_declaration

      (** [create_option short_name short_details long_details ty default_value]
          creates an option for the configuration file and the command-line
          arguments. *)
      val create_option :
        string ->
        string ->
        string ->
        'a SimpleConfig.option_class ->
        'a ->
        'a SimpleConfig.config_option

      (** [MakeWarnings] is a functor which allows to register the warnings
          automatically in the global data structure [plugins] with the
          associated plugin and linter. *)
      module MakeWarnings :
        functor (WA : Lint_warning_types.WARNINGARG) ->
        sig
          type t = WA.t
          val report : Location.t -> t -> unit
          val report_file : string -> t -> unit
          val report_file_line : string -> int -> t -> unit
          val report_file_line_col : string -> int -> int -> t -> unit
        end

      (** Input functors which are used to register a main function to the
          linter. This functor is organized to allow to take a specific input
          for the main function. *)
      module MakeInputStructure : functor (I : Lint_input.STRUCTURE) ->  sig end
      module MakeInputInterface : functor (I : Lint_input.INTERFACE) ->  sig end
      module MakeInputToplevelPhrase : functor (I : Lint_input.TOPLEVEL) -> sig end
      module MakeInputCMT : functor (I : Lint_input.CMT) -> sig end
      module MakeInputML : functor (I : Lint_input.ML) -> sig end
      module MakeInputMLI : functor (I : Lint_input.MLI) -> sig end
      module MakeInputAll : functor (I : Lint_input.ALL) -> sig end
      module MakeInputTokens : functor (I : Lint_input.TOKENS) -> sig end
    end
end
