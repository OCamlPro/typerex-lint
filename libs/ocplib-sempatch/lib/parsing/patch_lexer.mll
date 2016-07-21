{
  open Patch_parser

  let str_litteral_buf = Buffer.create 30
}

let white = [' ' '\t']+
let newline = ('\r' | '\n' | "\r\n")
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let number = [ '0'-'9' ]+
let title_delim = '@'
let code_delim = "```"

let comment_begin = "(*"
let comment_end = "*)"

rule read =
  parse
  | white { read lexbuf }
  | newline { Lexing.new_line lexbuf; EOL }
  | code_delim newline {
    Lexing.new_line lexbuf;
    Buffer.clear str_litteral_buf; read_code lexbuf;
    CODE (Buffer.to_bytes str_litteral_buf |> Bytes.to_string)
  }
  | "expressions" { EXPR_KW }
  | "name" { NAME_KW }
  | "when" { GUARD_KW }
  | "\"" {
    Buffer.clear str_litteral_buf; read_string lexbuf;
    STRING (Buffer.to_bytes str_litteral_buf |> Bytes.to_string)
  }
  | ':' { COLON }
  | ',' { COMMA }
  | title_delim { TITLE_DELIM }
  | id { ID (Lexing.lexeme lexbuf) }
  | number { NUMBER (int_of_string @@ Lexing.lexeme lexbuf) }
  | comment_begin { read_comment lexbuf; read lexbuf }
  | eof { EOF }
  | _ { raise (Failure.SempatchException
      (Failure.Lexing Lexing.(lexbuf.lex_curr_p))) }

and read_code =
  parse
  | newline code_delim { Lexing.new_line lexbuf; () }
  | newline { Lexing.new_line lexbuf;
              Buffer.add_string str_litteral_buf (Lexing.lexeme lexbuf);
              read_code lexbuf }
  | _ { Buffer.add_string str_litteral_buf (Lexing.lexeme lexbuf);
        read_code lexbuf }

and read_string =
  parse
  | '"' { () }
  | '\\' '"' { Buffer.add_char str_litteral_buf '"'; read_string lexbuf }
  | _ { Buffer.add_string str_litteral_buf (Lexing.lexeme lexbuf);
        read_string lexbuf }

and read_comment =
  parse
  | comment_end { () }
  | newline { Lexing.new_line lexbuf; read_comment lexbuf }
  | _ { read_comment lexbuf }
