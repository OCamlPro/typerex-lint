{
  module G = Guard
  open Guard_parser
}

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let number = '-'? [ '0' - '9' ]*
let infix_symbol = [ '=' '|' '&' ]*
let open_paren = '('
let close_paren = ')'
let comma = ','
let space = [ ' ' '\t' ]

rule read =
  parse
  | space { read lexbuf }
  | id { ID (Lexing.lexeme lexbuf) }
  | number { NUMBER (int_of_string (Lexing.lexeme lexbuf)) }
  | infix_symbol { INFIX_OP (Lexing.lexeme lexbuf) }
  | open_paren { OPENING_PAREN }
  | close_paren { CLOSING_PAREN }
  | comma { COMMA }
  | eof { EOF }
