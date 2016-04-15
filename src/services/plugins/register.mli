
(* Register a [Warning.t] during an analyse. *)
module type CW = sig
  val register : Check.t -> Warning.t -> unit
end

(* Register a [Checks.t] in a plugin. *)
module type PC = sig
  val register : Plugin.t -> Check.t -> unit
end
