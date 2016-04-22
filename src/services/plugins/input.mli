type input =
    | InStruct of (Parsetree.structure -> unit)
    | InInterf of (Parsetree.signature -> unit)
    | InTop of (Parsetree.toplevel_phrase -> unit)
    | InCmt of (Cmt_format.cmt_infos -> unit)
    | InMl of (string -> unit)
    | InMli of (string -> unit)
    | InAll of (string list -> unit)

module type INPUT = sig val input : input end
module type STRUCTURE = sig val main : Parsetree.structure -> unit end
module type INTERFACE = sig val main : Parsetree.signature -> unit end
module type TOPLEVEL = sig val main : Parsetree.toplevel_phrase -> unit end
module type CMT = sig val main : Cmt_format.cmt_infos -> unit end
module type ML = sig val main : string -> unit end
module type MLI = sig val main : string -> unit end
module type ALL = sig val main : string list -> unit end
