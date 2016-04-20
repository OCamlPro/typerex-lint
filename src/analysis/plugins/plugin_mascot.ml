let details = "A style-checker for OCaml sources (code, documentation, \
interface, metrics, and typography)."

module PluginMascot = Plugin.MakePlugin (struct
    let name = "Mascot"
    let short_name = "plugin-mascot"
    let details = details
  end)
