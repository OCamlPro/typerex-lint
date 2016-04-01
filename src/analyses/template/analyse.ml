

module type CHECK = sig
  val analyse : (module TypedtreeIter.IteratorArgument)
  val info : Info.t
  val reports : Reports.t
end
