open Parsed_patches.Type

let apply patch expr =
  Eval.apply patch.header.name patch.body
    (Parsed_patches.preprocess_src_expr expr)
