%token EOF
%token EOL
%token COLON
%token COMMA
%token<string> ID

%token<string> CODE

(* %token <Raw_patch.patch_line list> OCAML_CODE *)

%token HASH
%token VARIABLE_KW

%start <(string * Parsed_patches.t) list> sempatch
%%

sempatch:
  | patches = list(patch) EOF { patches }

patch:
  | name = patch_name; header = patch_header; body = patch_body { let open Parsed_patches in name, {header; body} }

patch_name:
  | HASH name = ID EOL { name }

patch_header:
  |  vars = loption(vars_def) { { Parsed_patches.meta_expr = vars; Parsed_patches.meta_bindings = []; } }

vars_def:
  | VARIABLE_KW COLON vars = separated_nonempty_list(COMMA, ID) EOL { vars }

patch_body:
  | cde = CODE EOL { Raw_patch.to_patch_body (Code_parser.code Code_lexer.read_code (Lexing.from_string cde)) }
