type id = string

type header = {
  expr_variables : string list;
}

type body = {
  before: Parsetree.expression;
  after: Parsetree.expression;
}

type t = {
  name: id;
  header: header;
  body: body;
}
