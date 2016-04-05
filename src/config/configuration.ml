type t = {
  (* Analyse code_identifier_length *)
  code_identifier_len : bool;
  min_identifier_len : int;
  max_identifier_len : int
  (* Analyse XXXX *)
}

exception Wrong_field of string
exception Wrong_type of string
exception Wrong_format of string

let default = {
  code_identifier_len = true;
  min_identifier_len = 2;
  max_identifier_len = 15;
}

let safe_bool_of_string field default_value value line_nbr =
  try bool_of_string value
  with _ ->
    let msg =
      Printf.sprintf
        "Error in config file at line \'%i\' : %s needs a bool. \'%s\' is not a bool."
        line_nbr
        field
        value in
    raise (Wrong_type msg)

let safe_int_of_string field default_value value line_nbr =
  try int_of_string value
  with _ ->
    let msg =
      Printf.sprintf
        "Error in config file at line \'%i\' : %s needs an int. \'%s\' is not an int."
        line_nbr
        field
        value in
    raise (Wrong_type msg)

let update_config config field value line_nbr = match field with
  | "code_identifier_len" ->
    let b =
      safe_bool_of_string field default.code_identifier_len value line_nbr in
    { config with code_identifier_len = b }
  | "min_identifier_len" ->
    let i =
      safe_int_of_string field default.min_identifier_len value line_nbr in
    { config with min_identifier_len = i }
  | "max_identifier_len" ->
    let i =
      safe_int_of_string field default.max_identifier_len value line_nbr in
    { config with max_identifier_len = i }
  | _ ->
    let msg =
      Printf.sprintf
        "Error in config file at line \'%i\' : Unknown field \'%s\'%!"
        line_nbr
        field in
    raise (Wrong_field msg)

let read_field config line line_nbr =
  try Scanf.sscanf line " (* %s@)" (fun _str -> ()); config
  with _ ->
  try
    let field, value = Scanf.sscanf line " %s = %s" (fun str v -> str, v) in
    update_config config field value line_nbr
  with
  | End_of_file -> config
  | Wrong_type msg as e -> raise e
  | Wrong_field msg as e -> raise e
  | _ ->
    let msg =
      Printf.sprintf
        "Error in config file at line %i : format is \'key = value\'%!"
        line_nbr in
    raise (Wrong_format msg)

let read_config file =
  let ic = open_in file in
  let rec acc config line_nbr =
    try
      let line = input_line ic in
      let config = read_field config line line_nbr in
      acc config (line_nbr + 1)
    with End_of_file -> config in
  acc default 1

let print_config ppf config =
  Format.fprintf ppf "%s\n%!"  "(* Code : Identifier length *)";
  Format.fprintf ppf "%s = %b\n%!"  "code_identifier_len"
    config.code_identifier_len;
  Format.fprintf ppf "%s = %i\n%!"  "min_identifier_len"
    config.min_identifier_len;
  Format.fprintf ppf "%s = %i\n%!"  "max_identifier_len"
    config.max_identifier_len
