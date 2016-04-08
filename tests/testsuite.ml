let run_command prog args exe_name dir =
  let file_out_name = Printf.sprintf "%s.out" exe_name in
  let file_err_name = Printf.sprintf "%s.err" exe_name in
  let file_stdout = Filename.concat dir file_out_name in
  let file_stderr = Filename.concat dir file_err_name in
  let fd_stdout = Unix.openfile file_stdout [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let fd_stderr = Unix.openfile file_stderr [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let pid = Unix.create_process prog args Unix.stdin fd_stdout fd_stderr in
  Unix.close fd_stdout;
  Unix.close fd_stderr;
  let rpid, status = Unix.waitpid [] pid in
  assert(rpid = pid);
  status

let run_diff dir =
  let file1 = Filename.concat dir "output.txt" in
  let file2 = Filename.concat dir "expected.txt" in
  let prog = "diff" in
  let args = [| prog; "-q"; file1; file2 |] in
  let cmd = Array.fold_left (fun acc arg -> acc ^ arg ^ " ") "" args in
  let status = run_command prog args prog dir in
  match status with
  | Unix.WEXITED 0 -> true
  | Unix.WEXITED 1 -> false
  | Unix.WEXITED n ->
    (Printf.eprintf "Command return code %i:\n  %s\n%!" n cmd; false)
  | Unix.WSIGNALED n ->
    (Printf.eprintf "Command killed with signal %i:\n  %s\n%!" n cmd; false)
  | Unix.WSTOPPED _n -> false
    
let check_test dir =
  (* Format.eprintf "  Running check in %S\n%!" dir; *)
  run_diff dir, dir

let check_tests test_dirs =
  (* Format.eprintf "Running checks in '%d' test dir(s)\n%!" (List.length test_dirs); *)
  List.map check_test test_dirs

let command_to_string args =
  Array.fold_left (fun acc arg -> Printf.sprintf "%s %s" acc arg) "" args

let run_ocp_lint dir warn_number =
  let prog = "../_obuild/ocp-lint/ocp-lint.asm" in
  let project_arg = "--project" in
  let json_arg = "--txt" in
  let json_file = Filename.concat dir "output.txt" in
  let warning_arg = "--warnings" in
  let warning = Printf.sprintf "-a+%i" warn_number in
  let args = [| prog; project_arg; dir; json_arg; json_file; warning_arg; warning |] in
  let cmd = Array.fold_left (fun acc arg -> acc ^ arg ^ " ") "" args in
  let status = run_command prog args "ocp-lint" dir in
  match status with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
    Printf.eprintf "Command return code %i:\n%s\n%!" n cmd;
    assert false
  | Unix.WSIGNALED n ->
    Printf.eprintf "Command killed with signal %i:\n%s\n%!" n cmd;
    assert false
  | Unix.WSTOPPED _n -> assert false

let to_lowercase str = String.lowercase str

let to_test_name check_name =
  let name = to_lowercase check_name in
  Str.global_replace (Str.regexp " ") "_" name

let get_warning_number dir =
  let list = 
    List.filter (fun chk -> 
        let info = Check_types.get_info chk in
        let name = to_test_name info.Info.name in
        name = dir)
      Checks.checks in
  if List.length list <> 1
  then (* TODO : Look for config *) 
    (Printf.eprintf "can't find warning number for %s\n%!" dir; assert false)
  else fst (List.hd list)

let run_test dir =
  (* Format.eprintf "  Running test in %S\n%!" dir; *)
  let warn_number = get_warning_number dir in
  run_ocp_lint dir warn_number

let run_tests test_dirs =
  (* Format.eprintf "Running tests in '%d' test dir(s)\n%!" (List.length test_dirs); *)
  List.iter run_test test_dirs

let find_test_dirs () =
  let dir = "." in
  let files = Sys.readdir dir in
  List.filter (fun file ->
      let dirname = Filename.concat dir file in
      Sys.is_directory dirname && file <> "_obuild")
    (Array.to_list files)

let print_res res =
  List.iter (fun (diff, dir) ->
      if diff
      then Printf.printf "  [PASS] %S\n%!" dir
      else Printf.printf "  [FAIL] %S\n%!" dir)
    res;
  let succeed = List.filter (fun (diff, _) -> diff) res in
  Printf.printf "%i/%i tests passed!\n%!" (List.length succeed) (List.length res)

let _ =
  Printf.printf "Running tests...\n%!";
  let test_dirs = find_test_dirs () in
  run_tests test_dirs;
  let res = check_tests test_dirs in
  print_res res
