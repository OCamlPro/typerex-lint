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
open Lint_db_types

type failure =
  | No_db of string
  | Empty_db of string
  | Diff_warnings of ((Lint_warning_types.warning list) *
                      (Lint_warning_types.warning list)) list
  | Diff_db of string list

type res =
  | Ok
  | Fail of failure

let (//) = Filename.concat

(* Testsuite files *)
let out = "ocp-lint.out"
let err = "ocp-lint.err"
let result = "ocp-lint.result"
let sempatch_file = "sempatch.md"
let failure_report = "testsuite/failure.desc"

(* Testsuite programs *)

let read_db file = try Lint_db.DefaultDB.load file with _ -> Hashtbl.create 42

let string_of_diff indent = function
| Ok -> indent ^ "Ok"
| Fail f -> begin match f with
    | No_db f -> Printf.sprintf "%sNo expected db %S" indent f
    | Empty_db f -> Printf.sprintf "%sExpected db is empty : %S" indent f
    | Diff_db l ->
      Printf.sprintf
        "%sDifferences in these plugins : %s"
        indent
        (String.concat ";" l)
    | Diff_warnings l -> indent ^"Differences of warnings"
  end

let fail_warnings res w1 w2 = match res with
  | Ok -> Fail (Diff_warnings [(w1, w2)])
  | Fail (Diff_warnings l) -> Fail (Diff_warnings ((w1, w2) :: l))
  | _ -> res

let fail_db res p = match res with
  | Ok -> Fail (Diff_db [p])
  | Fail (Diff_db l) -> Fail (Diff_db (p::l))
  | _ -> res

let is_included file_res olint_dir =
  if not (Sys.file_exists file_res) then (Fail (No_db file_res))
  else
    let db1 = read_db (Filename.concat olint_dir "db") in
    let db2 = read_db file_res in
    if Hashtbl.length db2 = 0 then (Fail (Empty_db file_res))
    else
      Hashtbl.fold (fun file (_hash, fres2) acc ->
          StringMap.fold (fun pname lres2 acc ->
              StringMap.fold (fun lname (_, _, _, wres2) acc ->
                  let (_, fres1) =
                    try
                      Hashtbl.find db1 file
                    with Not_found -> failwith file in
                  let pres1 =
                    try
                      StringMap.find pname fres1
                    with Not_found -> failwith pname in
                  let (_, _, _, wres1) =
                    try
                      StringMap.find lname pres1
                    with Not_found -> failwith lname in

                  (* TODO dépend de l'ordre, il faut changer ça *)
                  let l =
                     (List.length wres1) = (List.length wres2) in
                  let flag =
                    List.for_all (fun w1 ->
                        try
                          ignore (List.find (fun w2 ->
                              Lint_warning.cmp_warnings w1 w2) wres2);
                          true
                        with Not_found -> false
                      ) wres1
                  in

                  if flag && l then acc
                  else fail_warnings acc wres1 wres2)
                lres2 acc)
            fres2 acc)
        db2 Ok

let run_command prog args dir  =
  let file_stdout = dir // out in
  let file_stderr = dir // err in
  let fd_stdout =
    Unix.openfile
      file_stdout
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let fd_stderr =
    Unix.openfile
      file_stderr
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o644 in
  let pid = Unix.create_process prog args Unix.stdin fd_stdout fd_stderr in
  Unix.close fd_stdout;
  Unix.close fd_stderr;
  let rpid, status = Unix.waitpid [] pid in
  assert (rpid = pid);
  status

let check_tests test_dirs olint_dir =
  List.map (fun dir ->
    let db_path = Filename.concat dir "ocp-lint.result" in
    is_included db_path olint_dir, dir) test_dirs

let starts_with str ~substring =
  let len_sub = String.length substring in
  str = (String.sub str 0 len_sub)

let run_ocp_lint ocplint dir =
  let project_arg = "--path" in
  if Sys.file_exists (dir // sempatch_file)
  then begin
    try
      Unix.putenv "OCPLINT_PATCHES" dir
        with _ -> ()
  end;
  let args = [| ocplint; project_arg; dir |] in
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
        Sys.is_directory dirname && file <> "_olint")
      (Array.to_list files) in
  List.map (fun file -> parent // file) files

let print_result res verbose =
  let fail_oc = if verbose then stdout else open_out failure_report in
  List.iter (fun (diff, dir) ->
      if diff = Ok
      then Printf.printf "  \027[32m[PASS]\027[m %S\n%!" dir
      else
        (Printf.printf "  \027[31m[FAIL]\027[m %S\n%!" dir;
         Printf.fprintf fail_oc "%s\n%!" (string_of_diff "      " diff))
    )
    res;
  let succeed = List.filter (fun (diff, _) -> diff = Ok) res in
  let n_succ = List.length succeed in
  let n_res = List.length res in
  Printf.printf "%i/%i tests passed!\n%!" n_succ n_res;
  if n_succ = n_res then exit 0
  else exit 1

let _ =
  if Array.length Sys.argv < 3 then
    failwith "Usage: testsuite OCPLINTBINARY PATH";
  let ocplint = Sys.argv.(1) in
  let parent = Sys.argv.(2) in
  let olint_dir = Filename.concat parent "_olint" in
  let db_file = Filename.concat olint_dir "db" in
  if not (Sys.file_exists olint_dir) then Unix.mkdir olint_dir 0o755;
  if (Sys.file_exists db_file) then Sys.remove db_file;
  Printf.printf "Running tests...\n%!";
  let test_dirs = find_directories parent in

  (* Starting ocp-lint on each subdirectories. *)
  List.iter (run_ocp_lint ocplint) test_dirs;
  let result = check_tests test_dirs olint_dir in
  print_result result true
