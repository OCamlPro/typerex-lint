type kind =
  | Code
  | Typo
  | Interface
  | Metrics
  | Custom of string

and kinds = kind list

type warning = {
  id : int;
  kinds : kinds;
  short_name : string;
  message : string;
  loc : Location.t;
}

module type WarningArg = sig
  type t
  val report : Location.t -> t -> unit
end
