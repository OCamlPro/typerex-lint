
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

let print reports =
  StringSet.iter (fun report ->
      if report.loc <> Location.none then
        Format.eprintf "%a\n  %s\n" Location.print_loc report.loc report.msg
      else
        Format.eprintf "%s\n" report.msg)
    !reports
