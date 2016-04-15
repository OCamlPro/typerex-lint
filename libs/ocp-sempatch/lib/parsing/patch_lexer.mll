{
  open Patch_parser

  let code_buf = Buffer.create 30
}

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ocaml_code = [^'+' '-' '\n'] [^'\n']*

rule read =
  parse
  | white { read lexbuf }
  | newline* { EOL }
  | "```" { Buffer.clear code_buf; read_quoted lexbuf; CODE (Buffer.to_bytes code_buf |> Bytes.to_string) }
  | "variables" { VARIABLE_KW }
  | ':' { COLON }
  | ',' { COMMA }
  | '#' { HASH }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }

and read_quoted =
  parse
  | newline "```" { () }
  | _ { Buffer.add_string code_buf (Lexing.lexeme lexbuf); read_quoted lexbuf }

{
  let read_all = read
    (* let in_code = ref false in *)
    (* function x -> *)
    (*   let read_fun = *)
    (*   if !in_code then *)
    (*     read_quoted *)
    (*   else *)
    (*     read *)
    (*   in *)
    (*   match read_fun x with *)
    (*   | TICKS -> in_code := not !in_code; TICKS *)
    (*   | res -> res *)
}
