open Warning_types

let kind_code = Code
let kind_typo = Typo
let kind_interface = Interface
let kind_metrics = Metrics

let new_kind kind = Custom kind

let kind_to_string = function
  | Code -> "code"
  | Typo -> "typographie"
  | Interface -> "interface"
  | Metrics -> "metrics"
  | Custom kind -> kind

(* Warning Set *)
module WarningSet = Set.Make (struct
    type t = warning
    let compare = Pervasives.compare
  end)

type t = WarningSet.t ref

let empty = ref WarningSet.empty

let add_warning warning wset =
  wset := WarningSet.add warning !wset

let add loc id kinds short_name message wset =
  let warning = { id; kinds; short_name; message; loc } in
  add_warning warning wset

let length wset = WarningSet.cardinal !wset

let iter f wset = WarningSet.iter f !wset

let print ppf warning =
  if warning.loc <> Location.none then
    Format.fprintf ppf "%a" Location.print warning.loc;

  Format.fprintf ppf "  %s" warning.message;
  Format.fprintf ppf "@."
