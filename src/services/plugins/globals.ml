module LintMap = Map.Make (String)

module Config = Configuration.DefaultConfig

let plugins = Hashtbl.create 42
