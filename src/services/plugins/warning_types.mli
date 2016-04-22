(** [kind] is the category of the warning. A warning can be about the code, a
    typography (for example the line length), interface, metrics and custom. *)
type kind =
  | Code
  | Typo
  | Interface
  | Metrics
  | Custom of string

and kinds = kind list

type warning = {
  id : int;            (* Warning number *)
  kinds : kinds;       (* Warning kinds *)
  short_name : string; (* A short name to identify a warning *)
  message : string;    (* The displayed message *)
  loc : Location.t;    (* The location of the warning *)
}

module type WarningArg = sig
  type t
  val report : Location.t -> t -> unit
end
