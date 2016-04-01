
type kind = Warning | Error

type report = {
  kind : kind;
  loc : Location.t;
  check : Info.t;
  msg : string;
}

let none =
  let open Info in
  { kind = Warning;
    loc = Location.none;
    check = {name = "_none_"; details = "..."; cat = Code};
    msg = ""
  }

module StringSet = Set.Make (
  struct
    type t = report
    let compare = Pervasives.compare
  end)

type t = StringSet.t

let empty = StringSet.empty

let add = StringSet.add

let print_warnings reports =
  assert false
