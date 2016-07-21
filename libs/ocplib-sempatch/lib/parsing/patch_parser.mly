%token EOF
%token EOL
%token COLON
%token COMMA
%token<string> ID

%token<string> CODE

%token TITLE_DELIM

%token EXPR_KW
%token NAME_KW
%token GUARD_KW
%token<string> STRING
%token<int> NUMBER

%start <(string * Parsed_patches.unprocessed_patch) list> sempatch
%%

sempatch:
  | eols_option patches = list(patch) EOF { patches }

patch:
  | header = patch_header; body = patch_body
  { let open Parsed_patches in header.Parsed_patches.name, {unprocessed_header = header; unprocessed_body = body} }

patch_header:
  | TITLE_DELIM name = ID eols fields = list(header_def)
  { Parsed_patches.header_from_list (Parsed_patches.Name name :: fields) }

header_def:
  | EXPR_KW COLON exprs = separated_nonempty_list(COMMA, ID) eols
  { Parsed_patches.Expressions exprs }
  | NAME_KW COLON msg = string_or_id eols { Parsed_patches.Name msg }
  | GUARD_KW COLON guard = string_or_id eols
  { Parsed_patches.Guard
  (Guard_parser.guard Guard_lexer.read (Lexing.from_string guard))
  }
  | key = ID COLON value = string_or_id eols { Parsed_patches.KeyVal (key, value) }

patch_body:
  | cde = CODE eols
  { Raw_patch.to_patch_body
    (Code_parser.code Code_lexer.read_code (Lexing.from_string cde))
  }

eols:
  | nonempty_list(EOL) { }

eols_option:
  | list(EOL) { }

string_or_id:
  | s = STRING { s }
  | s = ID { s }
  | n = NUMBER { string_of_int n }
