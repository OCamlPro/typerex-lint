
type action =
| ActionNone
| ActionList
| ActionLoad of string

let action = ref ActionNone

let set_action new_action =
   if !action <> ActionNone then
     raise @@ Arg.Bad
       "Options --project or --list-warnings cannot be used together";
   action := new_action

let usage_msg =
  let name = Filename.basename Sys.argv.(0) in
  String.concat "\n" [
    "Usage:";
    Printf.sprintf "   %s [OPTIONS] --project DIR" name;
    Printf.sprintf "   %s [OPTIONS] --project DIR --warnings -4,-5" name;
    "";
  ]

let core_args_spec = Arg.align [
    "--project", Arg.String (fun dir -> set_action (ActionLoad dir)),
    "DIR   Give a project dir path";

    "--list-warnings", Arg.Unit (fun () -> set_action ActionList),
    " List of warnings";
  ]

let main () =
  (* Getting all options declared in all registered plugins. *)
  let all_args =
    Hashtbl.fold (fun plugin _ args ->
        let module Plugin = (val plugin : Plugin_types.PLUGIN) in
        Plugin.Config.simple_args () @ args)
      Plugin.plugins core_args_spec in

  Arg.parse all_args
    (fun s ->
       Printf.printf "Error: don't know what to do with %s\n%!" s;
       exit 1)
    usage_msg;

  match !action with
  | ActionLoad dir ->
    Ocplint_actions.scan ~filters:"" dir
  | ActionList ->
    exit 0
  | ActionNone ->
    Arg.usage all_args usage_msg;
    exit 0

let () = main ()
