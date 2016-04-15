
type kind = Warning | Error

type report = {
  kind : kind;
  loc : Location.t;
  info : Info.t;
  msg : string;
}

let none =
  let open Info in
  { kind = Warning;
    loc = Location.none;
    info = {name = "_none_"; details = "..."; cat = Code};
    msg = ""
  }

module StringSet = Set.Make (
  struct
    type t = report
    let compare = Pervasives.compare
  end)

type t = StringSet.t ref

let empty = ref StringSet.empty

let warning loc info msg = { kind = Warning; loc; info; msg }
let error loc info msg = { kind = Error; loc; info; msg }

let add report reports =
  reports := StringSet.add report !reports

let string_of_kind = function
  | Warning -> "Warning"
  | Error -> "Error"

let filter_by_kind kind reports =
  StringSet.filter (fun el -> el.kind = kind) !reports

let filter_by_cat cat reports =
  StringSet.filter (fun el -> Info.(el.info.cat) = cat) !reports

let output ppf reports =
  let open Info in
  let code = filter_by_cat Code reports in
  let typo = filter_by_cat Typo reports  in
  let interface = filter_by_cat Interface reports in

  if not (StringSet.is_empty code) then begin
    Format.fprintf ppf " --- Code ---\n%!";
    StringSet.iter (fun report ->
        if report.loc <> Location.none then
          Format.fprintf ppf "%a\n  %s\n"
            Location.print_loc report.loc report.msg
        else
          Format.fprintf ppf "%s\n" report.msg)
      code
  end;

  if not (StringSet.is_empty typo) then begin
    Format.fprintf ppf " --- Typography ---\n%!";
    StringSet.iter (fun report ->
        if report.loc <> Location.none then
          Format.fprintf ppf "%a\n  %s\n"
            Location.print_loc report.loc report.msg
        else
          Format.fprintf ppf "%s\n" report.msg)
      typo
  end;

  if not (StringSet.is_empty interface) then begin
    Format.fprintf ppf " --- Interface ---\n%!";
    StringSet.iter (fun report ->
        if report.loc <> Location.none then
          Format.fprintf ppf "%a\n  %s\n"
            Location.print_loc report.loc report.msg
        else
          Format.fprintf ppf "%s\n" report.msg)
      interface
  end;
  Format.fprintf ppf "%!"
