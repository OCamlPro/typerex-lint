open SimpleConfig (* for !! *)

(* We will register this linter to the Mascot plugin. *)
module Mascot = Plugin_mascot.PluginMascot

let details = "Checks long lines in your code. The default value is 80."

module CodeLength = Mascot.MakeLint(struct
    let name = "Code Length"
    let short_name = "code_length"
    let details = details
  end)

(* Defining/Using option from configuration file / command line *)
let max_line_length = CodeLength.create_option
    "max_line_length"
    "Maximum line length"
    "Maximum line length"
    SimpleConfig.int_option 80

type warning = LongLine of int

module Warnings = CodeLength.MakeWarnings(struct
    type t = warning

    let line_too_long loc args = CodeLength.new_warning
        loc
        1
        [ Warning.kind_code ]
        ~short_name:"long_line"
        ~msg:"This line is too long ('%line'): it should be at most of size '%max'."
        ~args

    let report loc = function
      | LongLine len ->
        line_too_long loc
          [("%line", string_of_int len);
           ("%max", string_of_int !!max_line_length)]
  end)

let check_line lnum file line =
  let line_len = String.length line in
  if line_len > !!max_line_length then
    let pos = Lexing.({dummy_pos with pos_fname = file; pos_lnum = lnum}) in
    let loc = Location.({none with loc_start = pos}) in
    Warnings.report loc (LongLine line_len)

let check_file file =
  let ic = open_in file in
  let rec loop lnum ic =
    try
      check_line lnum file (input_line ic);
      loop (lnum + 1) ic
    with End_of_file -> () in
  loop 1 ic;
  close_in ic

(* Registering a main entry to the linter *)
module MainSRC = CodeLength.MakeInputML(struct
    let main source = check_file source
  end)
