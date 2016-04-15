open SimpleConfig

(* EXEMPLE *)
module DummyPlugin = Plugin.MakePlugin (struct
    let name = "Plugin de test"
    let short_name = "dummy-test-plugin"
    let details = "This plugin is juste for test purpose."
  end )

module DummyLint = DummyPlugin.MakeCheck(struct
    let name = "Dummy check"
    let short_name = "dummy-test-check"
    let details = "This is a dummy analyse."
  end)


type warnings =
  | FirstWarning of string
  | SecondWarning of string

module DummyWarnings = DummyLint.MakeWarnings(struct
    type t = warnings
    let wlist = ref []
    let report loc = function
      | FirstWarning id | SecondWarning id -> wlist := id :: !wlist

    let warnings () = !wlist
  end)

let first_dummy_option = DummyLint.create_option
    "first-dummy-option"
    "Short First dummy option"
    "Long First dummy option"
    1
    SimpleConfig.int_option
    2

let second_dummy_option = DummyLint.create_option
    "second-dummy-option"
    "Second dummy option"
    "Long Second dummy option"
    1
    SimpleConfig.int_option
    15

let mapper =
  let open Parsetree in
  let open Asttypes in
  { Ast_mapper.default_mapper with
    Ast_mapper.pat  = fun mapper pat ->
      begin match pat.ppat_desc with
        | Ppat_var ident ->
          let id_str = ident.txt in
          let id_loc = ident.loc in
          let id_len = String.length id_str in
          if id_len < !!first_dummy_option then
            DummyWarnings.report id_loc (FirstWarning id_str);
          if id_len > !!second_dummy_option then
            DummyWarnings.report id_loc (SecondWarning id_str);
          pat
        | _ -> Ast_mapper.default_mapper.Ast_mapper.pat mapper pat
      end
  }


module MainSRC = DummyPlugin.MakeInputSRC(struct
    let main l =
      Printf.eprintf "source list\n%!"
  end)

module MainAST = DummyPlugin.MakeInputAST(struct
    let main ast =
      ignore (Ast_mapper.default_mapper.Ast_mapper.structure mapper ast)
  end)

module MainAST2 = DummyPlugin.MakeInputAST(struct
    let main ast = print_endline "deuxieme main"
  end)

module MainCMT = DummyPlugin.MakeInputCMT(struct
    let main cmt = ()
  end)

let () =
  Printf.eprintf "TOTO\n%!";
  let size = Hashtbl.length Plugin.mains in

  Printf.eprintf "MAIN %d\n%!" size;

  Hashtbl.iter (fun plugin checks ->
      List.iter (fun (check, runs) ->
          let runs =
            match runs with
            | None -> 0
            | Some runs -> (List.length runs) in
          Printf.eprintf "plugin: %S\n check= %S with %d mains\n%!"
            plugin check runs) checks)
    Plugin.mains
