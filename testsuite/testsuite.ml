(**************************************************************************)
(*                                                                        *)
(*                        OCamlPro Typerex                                *)
(*                                                                        *)
(*   Copyright OCamlPro 2011-2016. All rights reserved.                   *)
(*   This file is distributed under the terms of the GPL v3.0             *)
(*   (GNU General Public Licence version 3.0).                            *)
(*                                                                        *)
(*     Contact: <typerex@ocamlpro.com> (http://www.ocamlpro.com/)         *)
(*                                                                        *)
(*  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,       *)
(*  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES       *)
(*  OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND              *)
(*  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS   *)
(*  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN    *)
(*  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN     *)
(*  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE      *)
(*  SOFTWARE.                                                             *)
(**************************************************************************)

let (//) = Filename.concat

(* Testsuite files *)
let out = "ocp-lint.out"
let err = "ocp-lint.err"
let output = "ocp-lint.output"
let result = "ocp-lint.result"

(* Testsuite programs *)
let diff = "diff"

let run_command prog args dir  =
  let file_stdout = dir // out in
  let file_stderr = dir // err in
  let fd_stdout =
    Unix.openfile
      file_stdout
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o644 in
  let fd_stderr =
    Unix.openfile
      file_stderr
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_APPEND] 0o644 in
  let pid = Unix.create_process prog args Unix.stdin fd_stdout fd_stderr in
  Unix.close fd_stdout;
  Unix.close fd_stderr;
  let rpid, status = Unix.waitpid [] pid in
  assert (rpid = pid);
  status

let run_diff dir =
  let file1 = dir // output in
  let file2 = dir // result in
  let args = [| diff; "-q"; file1; file2 |] in
  let status = run_command diff args dir in
  match status with
  | Unix.WEXITED 0 -> true
  | Unix.WEXITED _
  | Unix.WSIGNALED _
  | Unix.WSTOPPED _ -> false

let check_tests test_dirs =
  let check_test dir = run_diff dir, dir in
  List.map check_test test_dirs

let run_ocp_lint ocplint dir =
  let output = dir // output in
  let project_arg = "--project" in
  let output_arg = "--output-txt" in
  let args = [| ocplint; project_arg; dir; output_arg; output |] in
  let status = run_command ocplint args dir in
  match status with
  | Unix.WEXITED 0   -> ()
  | Unix.WEXITED _   -> exit 1
  | Unix.WSIGNALED _ -> exit 2
  | Unix.WSTOPPED _  -> exit 3

let find_directories parent =
  let files = Sys.readdir parent in
  let files =
    List.filter (fun file ->
        let dirname =  parent // file in
        Sys.is_directory dirname)
      (Array.to_list files) in
  List.map (fun file -> parent // file) files

let print_result res =
  List.iter (fun (diff, dir) ->
      if diff
      then Printf.printf "  [PASS] %S\n%!" dir
      else Printf.printf "  [FAIL] %S\n%!" dir)
    res;
  let succeed = List.filter (fun (diff, _) -> diff) res in
  let n_succ = List.length succeed in
  let n_res = List.length res in
  Printf.eprintf "%i/%i tests passed!\n%!" n_succ n_res;
  if n_succ = n_res then exit 0
  else exit 1

let _ =
  if Array.length Sys.argv < 3 then
    failwith "Usage: testsuite OCPLINTBINARY PATH";
  let ocplint = Sys.argv.(1) in
  let parent = Sys.argv.(2) in
  Printf.eprintf "Running tests...\n%!" ;
  let test_dirs = find_directories parent in

  (* Starting ocp-lint on each subdirectories. *)
  List.iter (run_ocp_lint ocplint) test_dirs;

  let result = check_tests test_dirs in
  print_result result
