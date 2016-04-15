open Reports

(* Output reports - raw *)
let txt reports file =
  let oc = open_out file in
  output (Format.formatter_of_out_channel oc) reports;
  close_out oc

let print reports =
  output Format.err_formatter reports
