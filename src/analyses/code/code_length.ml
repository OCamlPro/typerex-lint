let reports = Reports.empty

module IteratorArg = struct
  include TypedtreeIter.DefaultIteratorArgument

  let enter_pattern p =
    let open Typedtree in
    match p.pat_desc with
    | Tpat_var (id, _) ->
      Format.eprintf "TEST %S\n%!" (Ident.name id)
    | _ -> ()
end

module Check : Analyse.CHECK = struct
  let analyse = (module IteratorArg : TypedtreeIter.IteratorArgument)
  let info = {
    Info.name = "name ";
    Info.details = "details";
    Info.cat = Info.Code;
  }
  let reports = reports
end

let check = (module Check : Analyse.CHECK)
