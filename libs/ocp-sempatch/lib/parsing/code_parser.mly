%token EOF
%token EOL
%token ENTER_SUBPATCH
%token EXIT_SUBPATCH
%token PLUS
%token MINUS
%token<string> CODE

%start <Raw_patch.t> code
%%

code:
  | option(EOL) lines = separated_list (EOL, code_line) EOF { lines }

code_line:
  | code = CODE { Raw_patch.EQUAL code }
  | PLUS code = CODE { Raw_patch.ADD code }
  | MINUS code = CODE { Raw_patch.REMOVE code }
  | ENTER_SUBPATCH sub = code EXIT_SUBPATCH { Raw_patch.SUBPATCH sub }
