type t = {
  (* Analyse code_identifier_length *)
  code_identifier_len : bool;
  min_identifier_len : int;
  max_identifier_len : int;

  (* Analyse code_length *)
  code_line_length : bool;
  max_line_len : int;

(* Analyse XXXX *)
}

(* Inputs *)
val default : t
val read_config : string -> t

(* Outputs *)
val dump_config : t -> string -> unit
val print_config : t -> unit
