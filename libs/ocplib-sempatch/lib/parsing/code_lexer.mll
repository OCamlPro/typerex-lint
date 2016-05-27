{
  open Code_parser
}


let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ocaml_code = [^'+' '-' '\n'] [^'\n']*

rule read_code =
  parse
  | "<..." newline { ENTER_SUBPATCH }
  | newline "...>" { EXIT_SUBPATCH }
  | newline* { EOL }
  | '+' { PLUS }
  | '-' { MINUS }
  | ocaml_code { CODE (Lexing.lexeme lexbuf) }
  | white { read_code lexbuf }
  | eof { EOF }
