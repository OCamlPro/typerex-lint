%token<string> ID
%token<string> INFIX_OP
%token<int> NUMBER
%token OPENING_PAREN
%token CLOSING_PAREN
%token COMMA
%token EOF

%start <Guard.t> guard
%%

guard:
  | expr = expr EOF { expr }

expr:
  | e1 = expr op = INFIX_OP e2 = expr { Guard.Apply ("(" ^ op ^ ")", [e1; e2]) }
  | fn = ID OPENING_PAREN
  args = separated_nonempty_list(COMMA, expr)
  CLOSING_PAREN
  { Guard.Apply (fn, args) }
  | OPENING_PAREN e = expr CLOSING_PAREN { e }
  | var = ID { Guard.Variable var }
  | number = NUMBER { Guard.Litt_integer number }
