module type INPUT = sig
  val main : 'a -> unit
end

type input =
    | AST of (Parsetree.structure -> unit)
    | CMT of (Cmt_format.cmt_infos -> unit)
    | SRC of (string list -> unit)

module type InputAST = sig
  val main : Parsetree.structure -> unit
end

module type InputCMT = sig
  val main : Cmt_format.cmt_infos -> unit
end

module type InputSRC = sig
  val main : string list -> unit
end
