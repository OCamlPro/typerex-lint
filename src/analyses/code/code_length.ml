open Asttypes
open Parsetree
open Ast_mapper
open Check_types
open Configuration
open Info
open Reports

let details =
  Printf.sprintf "Checks long lines in your code. The default value is %d.\n"
    Configuration.default.max_line_len

let info = {
  name = "Code Length";
  details;
  cat = Code;
}

let check_line config reports lnum file line =
  let line_len = String.length line in
  if line_len > config.max_line_len then
    let pos = Lexing.({dummy_pos with pos_fname = file; pos_lnum = lnum}) in
    let loc = Location.({none with loc_start = pos}) in
    let msg = Printf.sprintf "Line too long (%d)." line_len in
    Reports.add (Reports.warning loc info msg) reports

let check_file config reports file =
  let ic = open_in file in
  let rec loop lnum ic =
    try
      check_line config reports lnum file (input_line ic);
      loop (lnum + 1) ic
    with End_of_file -> () in
  loop 1 ic;
  close_in ic

let run config reports sources =
  List.iter (check_file config reports) sources

let check : Check_types.global_check = { global_run = run; global_info = info }
