module type WARNINGTYPES = sig
  type t
end

module type WARNINGARG = sig
  type t
  val report : Location.t -> t -> unit
  val report_file : string -> t -> unit
end


module DefaultWarning (TY : WARNINGTYPES) = struct
  type t = TY.t
  let report _ _ = ()
  let report_file _ _ = ()
end
