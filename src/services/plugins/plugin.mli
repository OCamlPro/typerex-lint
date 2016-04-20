module LintMap : sig
  type 'a t
  type key = string
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

val plugins : (string, (Input.input list) LintMap.t) Hashtbl.t
val iter_plugins : (string -> LintMap.key -> Input.input list -> unit) -> unit

module MakePlugin :
  functor (Plugin : Plugin_types.PluginArg) ->
  sig
    module Config : Configuration.CONFIG

    val name : string
    val short_name : string
    val details : string
    val warnings : Warning.t

    module MakeCheck :
      functor (CA : Check.CheckArg) ->
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

        module MakeWarnings :
          functor (WA : Warning_types.WarningArg) ->
          sig
            type t
            val report : Location.t -> WA.t -> unit
          end


        module MakeInputAST :
          functor (A : Input.InputAST) ->
          sig
            val main : Parsetree.structure -> unit
          end

        module MakeInputCMT :
          functor (C : Input.InputCMT) ->
          sig
            val main : Cmt_format.cmt_infos -> unit
          end

        module MakeInputSRC :
          functor (S : Input.InputSRC) ->
          sig
            val main : string list -> unit
          end
      end
  end
