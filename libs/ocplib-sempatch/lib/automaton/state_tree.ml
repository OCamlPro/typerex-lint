module StateI = State.Identifier

[%%create_state_tree]

let pp = fun fmt _ -> Format.pp_print_string fmt "<opaque>"
let show _ = "<opaque>"
