let info = {
  Info.name = "Code Identifier Length";
  Info.details = "Long details";
  Info.cat = Info.Code;
}

module IteratorArg = struct
  include TypedtreeIter.DefaultIteratorArgument

  let enter_pattern p =
    let open Typedtree in
    match p.pat_desc with
    | Tpat_var (id, _) ->
      let id_str = Ident.name id in
      let id_len = String.length id_str in
      let min_len = Global.min_identifier_len in
      let max_len = Global.max_identifier_len in
      if id_len < min_len then
        Printf.eprintf "%S is too short: it should be at least of size '%d'.\n%!"
          id_str
          min_len;
      if id_len > max_len then
        Printf.eprintf "%S is too long: it should not exceed '%d'.\n%!"
          id_str
          max_len
    | _ -> ()
end

module Check : Analyse.CHECK = struct
  let analyse = (module IteratorArg : TypedtreeIter.IteratorArgument)
  let info = info
  let reports = Reports.empty
end

let check = (module Check : Analyse.CHECK)
