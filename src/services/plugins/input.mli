module type INPUT = sig
  type t
end

module type InputAST = sig
  val main : Parsetree.structure -> unit
end

module type InputCMT = sig
  val main : Cmt_format.cmt_infos -> unit
end

module type InputSRC = sig
  val main : string list -> unit
end
