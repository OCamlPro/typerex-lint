module LintMap : sig
  type 'a t
  type key = string
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

val plugins :
  ((module Plugin_types.PLUGIN), (Input.input list) LintMap.t) Hashtbl.t
val iter_plugins :
  ((module Plugin_types.PLUGIN) -> (Input.input list) LintMap.t -> unit) ->
  unit

module MakePlugin : functor (Plugin : Plugin_types.PluginArg) ->
sig

  module Config : Configuration.CONFIG

  val name : string
  val short_name : string
  val details : string

  module MakeLint :
    functor (CA : Lint.LintArg) ->
    sig
      val name : string
      val short_name : string
      val details : string

      val new_warning :
        Location.t ->
        int ->
        Warning_types.kind list ->
        short_name:string ->
        msg:string ->
        args: (string * string) list ->
        unit

      val create_option :
        string ->
        string ->
        string ->
        'a SimpleConfig.option_class ->
        'a ->
        'a SimpleConfig.config_option

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
