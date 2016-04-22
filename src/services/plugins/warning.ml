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
module WarningSet = Set.Make (
  struct
    type t = warning
    let compare = Pervasives.compare
  end)

type t = WarningSet.t ref

let empty = ref WarningSet.empty

let add loc id kinds short_name message wset =
  let warning = { id; kinds; short_name; message; loc } in
  wset := WarningSet.add warning !wset

let add_warning warning wset =
  wset := WarningSet.add warning !wset

let iter f s = WarningSet.iter f !s

let print ppf warning =
  let kinds = String.concat " " (List.map kind_to_string warning.kinds) in
  if warning.loc <> Location.none then
    Format.fprintf ppf "%a" Location.print warning.loc;

  Format.fprintf ppf "   id: %d\n   kinds: [%s]\n   short: %s\n   msg: %s  \n%!"
    warning.id kinds warning.short_name warning.message;
  Format.fprintf ppf "@."
