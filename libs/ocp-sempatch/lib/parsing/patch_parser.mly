%token EOF
%token EOL
%token COLON
%token COMMA
%token<string> ID

%token <Raw_patch.patch_line list> OCAML_CODE

%token HASH
%token VARIABLE_KW

%start <Parsed_patches.t list> sempatch
%start <Parsed_patches.body> patch_body
%%

sempatch:
  | patches = list(patch) EOF { patches }

patch:
  | name = patch_name; header = patch_header; body = patch_body { let open Parsed_patches in {name; header; body} }

patch_name:
  | HASH name = ID EOL { name }

patch_header:
  |  vars = loption(vars_def) { { Parsed_patches.expr_variables = vars } }

vars_def:
  | VARIABLE_KW COLON vars = separated_nonempty_list(COMMA, ID) EOL { vars }

patch_body:
  | code = OCAML_CODE EOL { Raw_patch.to_patch_body code }
