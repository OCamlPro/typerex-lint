{
  open Patch_parser
}

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ocaml_code = [^'\n']*

rule read =
  parse
  | white { read lexbuf }
  | newline* { EOL }
  | "```" { read_code [] lexbuf }
  | "variables" { VARIABLE_KW }
  | ':' { COLON }
  | ',' { COMMA }
  | '#' { HASH }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }
and read_code lst =
  parse
  | "```" { OCAML_CODE (List.rev lst) }
  | newline { read_code lst lexbuf }
  | ocaml_code {read_code ((Lexing.lexeme lexbuf) :: lst) lexbuf }
  | eof { failwith "Non terminated code" }
