(** [LintArg] is a type module which is used by the functor [Plugin.MakeLint]. *)
module type LintArg = sig
  val name : string
  val short_name : string
  val details : string
end
