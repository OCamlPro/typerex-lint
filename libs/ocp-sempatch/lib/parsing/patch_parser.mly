%token EOF
%token EOL
%token COLON
%token COMMA
%token<string> ID

(* %token CODE_DELIM *)
(* %token MINUS *)
(* %token PLUS *)
%token <Raw_patch.body> OCAML_CODE

%token HASH
%token VARIABLE_KW

%start <Raw_patch.t list> patches
%start <Raw_patch.body> patch_body
%%

patches:
  | patches = separated_list(EOL, patch) EOF { patches }
  (* | patches = patch EOF { [patches] } *)

patch:
  | name = patch_name header = patch_header body = patch_body { (name, header, body) }

patch_name:
  | HASH name = ID EOL { name }

patch_header:
  | vars = loption(vars_def) { { Raw_patch.expr_variables = vars } }

vars_def:
  | VARIABLE_KW COLON vars = separated_nonempty_list(COMMA, ID) EOL { vars }

patch_body:
  | code = OCAML_CODE EOL { code }(* TODO *)
