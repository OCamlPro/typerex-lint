open! Parsetree

[%%create_eval_tree]

    let pp fmt = function
      | Expression e ->
        Pprintast.expression fmt e
      | Structure s ->
        Pprintast.structure fmt s
      | _ -> Format.pp_print_string fmt "<Opaque tree>"

    let show tree =
      let () = pp Format.str_formatter tree in
      Format.flush_str_formatter ()

let to_node = Convert.convert

let compare_nodes n1 n2 = compare (to_node n1) (to_node n2)
