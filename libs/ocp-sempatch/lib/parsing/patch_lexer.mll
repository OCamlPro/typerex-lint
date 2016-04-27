{
  open Patch_parser

  let str_litteral_buf = Buffer.create 30
}

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let ocaml_code = [^'+' '-' '\n'] [^'\n']*
let title_delim = '#'
let code_delim = "```"

rule read =
  parse
  | white { read lexbuf }
  | newline* { EOL }
  | code_delim newline {
    Buffer.clear str_litteral_buf; read_code lexbuf;
    CODE (Buffer.to_bytes str_litteral_buf |> Bytes.to_string)
  }
  | "expressions" { EXPR_KW }
  | "bindings" { BINDINGS_KW }
  | "message" { MESSAGES_KW }
  | "\"" {
    Buffer.clear str_litteral_buf; read_string lexbuf;
    STRING (Buffer.to_bytes str_litteral_buf |> Bytes.to_string)
  }
  | ':' { COLON }
  | ',' { COMMA }
  | title_delim { TITLE_DELIM }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }

and read_code =
  parse
  | newline code_delim { () }
  | _ { Buffer.add_string str_litteral_buf (Lexing.lexeme lexbuf); read_code lexbuf }

and read_string =
  parse
  | '"' { () }
  | '\\' '"' { Buffer.add_char str_litteral_buf '"'; read_string lexbuf }
  | _ { Buffer.add_string str_litteral_buf (Lexing.lexeme lexbuf); read_string lexbuf }
