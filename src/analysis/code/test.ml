open Code_identifier_length

let () =
  Printf.eprintf "TOTO\n%!";
  let size = Hashtbl.length Plugin.plugins in

  Printf.eprintf "MAIN %d\n%!" size;
  let parse source =
    Pparse.parse_implementation ~tool_name:"" Format.err_formatter source in

  MainAST.main
    (parse "/home/cago/dev/ocaml-style-checker/src/services/plugins/plugin.ml");

  (* MainSRC.main []; *)
  Plugin.iter_plugins (fun pname cname runs ->
      let sz = List.length runs in
      Printf.eprintf "plugin: %S\n check= %S with %d mains\n%!"
        pname cname sz);

  Warning.iter (fun warning ->
      Warning.print Format.err_formatter warning)
    Mascot.warnings
