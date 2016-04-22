module type PluginArg = sig
  val name : string
  val short_name : string
  val details : string
end

module type PLUGIN = sig
  module Config : Configuration.CONFIG
  val name : string
  val short_name : string
  val details : string
  val warnings : Warning.t
end
