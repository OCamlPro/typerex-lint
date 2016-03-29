module T = Toploop

let patch_file =
  let n = Array.length Sys.argv in
  if n < 3 then
    `Error "Expected more args"
  else
    `Ok Sys.argv.(n-3)

let void_formatter =
  Format.make_formatter (fun _ _ _ -> ()) (fun  () -> ())

let eval () =

  let file_in =
    match patch_file with
    | `Error e -> failwith e
    | `Ok f ->
      try
        open_in f
      with
        Sys_error _ ->
        failwith "Couldn't open patch file"
  in

  T.initialize_toplevel_env ();
  begin
  let findlib_dir = Findlib.package_directory "findlib" in
  Topdirs.dir_directory findlib_dir;
  Topdirs.dir_load Format.err_formatter (findlib_dir ^ Filename.dir_sep ^ "findlib.cma");
  Topdirs.dir_load Format.err_formatter (findlib_dir ^ Filename.dir_sep ^ "findlib_top.cma");
  T.execute_phrase false void_formatter (!T.parse_toplevel_phrase (Lexing.from_string "Topfind.log := fun _ -> ();;")) |> ignore;
  end;
  T.execute_phrase false void_formatter (!T.parse_toplevel_phrase (Lexing.from_string "Topfind.add_predicates [ \"byte\"; \"toploop\" ]; Topfind.don't_load [\"findlib\"];;")) |> ignore;

  T.execute_phrase false void_formatter (!T.parse_toplevel_phrase (Lexing.from_string "#require \"compiler-libs\";;")) |> ignore;
  T.execute_phrase false void_formatter (!T.parse_toplevel_phrase (Lexing.from_string "#require \"ppx_patch\";;")) |> ignore;
  List.map (T.execute_phrase false Format.std_formatter) (!T.parse_use_file (Lexing.from_channel file_in)) |> ignore;
  close_in file_in

let _ = eval ()
