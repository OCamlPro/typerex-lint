open Plugin_text

module Linter = Plugin.MakeLint(struct
    let name = "duplicate code"
    let version = "1"
    let short_name = "duplicated_code"
    let details =
      "Detect portions of code that duplicated"
    let enabled = true
  end)

type warning =
  | CodeDuplicated of string


let w_code_duplicated = Linter.new_warning
    ~id:1
    ~short_name:"dupli_code"
    ~msg:"Duplicated code detected in $file "
    ~severity:10


module Warnings = Linter.MakeWarnings(struct
    type t = warning

    let to_warning = function
      | CodeDuplicated name -> w_code_duplicated, ["file",name]
  end)

let read_entire_file filename = 
  let file = open_in filename in 
  let file_length = in_channel_length file in
  let str = Bytes.create file_length in
  really_input file str 0 file_length;
  close_in file;
  Bytes.unsafe_to_string(str)

let remove_char regexp replace str = 
  Str.global_replace regexp replace str


let rec get_length_file input_c acc = 
  let line = try [input_line input_c] with End_of_file -> [] in
  match line with
  | [] -> acc
  | [line] -> get_length_file input_c (1+acc)
  | _ -> failwith "weird things not a line"

let make_portions regex content = 
  let part = Str.split (Str.regexp regex) content in 
  List.map (remove_char (Str.regexp "[\n\t ]+") "") part

let hash_portions portions = 
  List.map (function e -> Digest.string e) portions

let rec check_duplicate acc l = match l with
  | [] -> acc
  | (x,p)::tl ->  if(p) then check_duplicate (1 + acc) tl else check_duplicate acc tl


let compare_ el l = List.map (function e -> (e,(el = e))) l


let rec if_duplicate_portions portions = match portions with
  | [] -> failwith "nothing to compare"
  | [em] -> false
  | hd::tl -> if((check_duplicate 0 (compare_ hd tl)) > 0) then true else if_duplicate_portions tl

let check_file file = 
  let portions = hash_portions (make_portions "^\\s*$" (read_entire_file file)) in
  if(if_duplicate_portions portions) then Warnings.report_file file (CodeDuplicated file)
  else ()

  module MainSRC = Linter.MakeInputML(struct
    let main source = check_file source
  end)
