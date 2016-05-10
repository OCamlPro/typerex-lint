open Parsed_patches.Type

let apply patch expr =
  Eval.apply patch.body expr
