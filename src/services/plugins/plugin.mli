
(** [iter_plugins f] applies f to all bindings in the global data structure
    [plugins]. [f] receives the [plugin] as first argument and the [LintMap.t]
    associated to this plugin as second. *)
val iter_plugins :
  ((module Plugin_types.PLUGIN) ->
   (Input.input list) Globals.LintMap.t -> unit) ->
  unit


(** [MakePlugin] is a functor which take a module of type
    [Plugin_types.PluginArg] as an argument. It register the plugin to the
    global [plugins] and all the linter associated and created with the functor
    [MakeLint]. *)
module MakePlugin : functor (Plugin : Plugin_types.PluginArg) ->
sig

  (** The name of the plugin.  *)
  val name : string
  (** The short name of the plugin. It must be unique for each plugin. *)
  val short_name : string
  (** The details of the plugin.  *)
  val details : string

  (** [MakeLintPatch] is a functor which take a module of type
      [Lint.LintPatchArg] as an argument. It allows to create a linter based
      on semantic patches. It takes the files names of patches as arguments and
      automatically register the linter linter into a plugin. *)
  module MakeLintPatch : functor (CA : Lint.LintPatchArg) ->  sig  end

  (** [MakeLint] is a functor which take a module of type [Lint.LintArg] as an
      argument. It allows to create a linter and automatically register it to
      the plugin in the global [plugins]. The functor allows to create specific
      options for each lint and register them to the [Config] of the associated
      plugin. *)
  module MakeLint :
    functor (CA : Lint.LintArg) ->
    sig
      (** The name of the lint. *)
      val name : string
      (** The short name of the lint. It must be unique for each linter. *)
      val short_name : string
      (** The details of the linter. *)
      val details : string

      (** [new_warning loc id kinds ~short_name ~msg ~args] register a warning
          with at the location [loc], the warning number [id], a short
          description [short_name], the message [msg] which will be displayed
          and the [args] which is a couple of string which will be substitute
          in the displayed message. *)
      val new_warning :
        Location.t ->
        int ->
        Warning_types.kind list ->
        short_name:string ->
        msg:string ->
        args: (string * string) list ->
        unit

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
      module MakeWarnings : functor (WA : Warning_types.WarningArg) ->
      sig
        type t
        val report : Location.t -> WA.t -> unit
      end

      (** Input functors which are used to register a main function to the
          linter. This functor is organized to allow to take a specific input
          for the main function. *)
      module MakeInputStructure : functor (S : Input.STRUCTURE) ->  sig end
      module MakeInputInterface : functor (A : Input.INTERFACE) ->  sig end
      module MakeInputToplevelPhrase : functor (A : Input.TOPLEVEL) -> sig end
      module MakeInputCMT : functor (C : Input.CMT) -> sig end
      module MakeInputML : functor (S : Input.ML) -> sig end
      module MakeInputMLI : functor (S : Input.MLI) -> sig end
    end
end
