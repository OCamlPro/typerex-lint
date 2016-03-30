module T = Toploop

let patch_file =
  let n = Array.length Sys.argv in
  if n < 3 then
    `Error "Expected more args"
  else
    `Ok Sys.argv.(n-3)

let open_patch () =
  match patch_file with
  | `Error _ as e -> e
  | `Ok f ->
    try
      `Ok (open_in f)
    with
      Sys_error _ ->
      `Error "Couldn't open patch file"

let void_formatter =
  Format.make_formatter (fun _ _ _ -> ()) (fun  () -> ())

let ocamlinit =
  "Topfind.log := fun _ -> ();;" ^
  "Topfind.add_predicates [ \"byte\"; \"toploop\" ]; Topfind.don't_load [\"findlib\"];;" ^
  "#require \"compiler-libs\";;" ^
  "#require \"ppx_patch\";;"
  (* For testing in local. TODO: Find a way to nicely handle this *)
  (* ^ "#directory \"lib\";;" *)
  (* ^ "#load_rec \"ppx_patch.cmo\";;" *)


let eval () =

  let file_in =
    match open_patch () with
    | `Ok f -> f
    | `Error e -> failwith e
  in

  T.initialize_toplevel_env ();
  begin
  let findlib_dir = Findlib.package_directory "findlib" in
  Topdirs.dir_directory findlib_dir;
  Topdirs.dir_load Format.err_formatter (findlib_dir ^ Filename.dir_sep ^ "findlib.cma");
  Topdirs.dir_load Format.err_formatter (findlib_dir ^ Filename.dir_sep ^ "findlib_top.cma");
  end;
  List.map (T.execute_phrase false Format.err_formatter) (!T.parse_use_file (Lexing.from_string ocamlinit)) |> ignore;
  List.map (T.execute_phrase false Format.err_formatter) (!T.parse_use_file (Lexing.from_channel file_in)) |> ignore;
  close_in file_in

let _ = eval ()
