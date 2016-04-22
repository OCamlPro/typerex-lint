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
  end

module DefaultConfig : CONFIG = MakeConfig(struct let filename = ".ocplint" end)
