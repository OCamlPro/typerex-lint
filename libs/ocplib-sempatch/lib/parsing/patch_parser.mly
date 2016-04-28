%token EOF
%token EOL
%token COLON
%token COMMA
%token<string> ID

%token<string> CODE

%token TITLE_DELIM

%token EXPR_KW
%token BINDINGS_KW
%token MESSAGES_KW
%token<string> STRING

%start <(string * Parsed_patches.t) list> sempatch
%%

sempatch:
  | eols_option patches = list(patch) EOF { patches }

patch:
  | name = patch_name; header = patch_header; body = patch_body
  { let open Parsed_patches in name, {header; body} }

patch_name:
  | TITLE_DELIM name = ID eols { name }

patch_header:
  | fields = list(header_def) { Parsed_patches.header_from_list fields }

header_def:
  | EXPR_KW COLON exprs = separated_nonempty_list(COMMA, ID) eols
  { Parsed_patches.Expressions exprs }
  | BINDINGS_KW COLON bindings = separated_nonempty_list(COMMA, ID) eols
  { Parsed_patches.Bindings bindings }
  | MESSAGES_KW COLON msg = STRING eols { Parsed_patches.Message msg }

patch_body:
  | cde = CODE eols
  { Raw_patch.to_patch_body
    (Code_parser.code Code_lexer.read_code (Lexing.from_string cde))
  }

eols:
  | nonempty_list(EOL) { }

eols_option:
  | list(EOL) { }
