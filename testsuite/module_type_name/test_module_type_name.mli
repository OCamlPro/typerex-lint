module type CORRECT = sig
  val value : string
end

module type NotCorrect = sig
  val value : string
end

module type NOT_cORRECT = sig
  val value : string
end
