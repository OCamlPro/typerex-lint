type 'a current = {
  replacement: 'a;
  substitutions: (string * Tree.t) list;
}
[@@deriving show]

type report = Tree.t * (Tree.t current)
[@@deriving show]

type 'a t = {
  reports: report list;
  current: 'a current;
}
[@@deriving show]

let mk_new tree = {
  reports = [];
  current = {
    replacement = tree;
    substitutions = [];
  }
}

let current_replacement t = t.current.replacement

let set_replacement replacement t =
  { t with current = { t.current with replacement } }

let merge mergereplace (e1 : 'a t) (e2 : 'b t) =
  {
    reports = e1.reports @ e2.reports;
    current = {
      replacement = mergereplace e1.current.replacement e2.current.replacement;
      substitutions = e1.current.substitutions @ e2.current.substitutions;
    }
  }

let map_replacement f e = set_replacement (f @@ current_replacement e) e
