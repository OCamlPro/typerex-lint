open Asttypes
open Parsetree
open Ast_mapper
open Check_types
open Configuration
open Info
open Reports

let info = {
  name = "Code Identifier Length";
  details = "Long details";
  cat = Code;
}

let mapper config reports =
  { default_mapper with
    pat  = fun mapper pat ->
      begin match pat.ppat_desc with
      | Ppat_var ident ->
        let id_str = ident.txt in
        let id_loc = ident.loc in
        let id_len = String.length id_str in
        let min_len = config.min_identifier_len in
        let max_len = config.max_identifier_len in
        if id_len < min_len then
          let msg =
            Printf.sprintf
              "%S is too short: it should be at least of size '%d'."
              id_str
              min_len in
          Reports.add (Reports.warning id_loc info msg) reports;
          if id_len > max_len then
            let msg =
              Printf.sprintf "%S is too long: it should not exceed '%d'.\n%!"
                id_str
                max_len in
            Reports.add (Reports.warning id_loc info msg) reports
      | _ -> () end;
      pat
  }

let run config reports source =
  ignore (check_source (mapper config reports) source)

let check : Check_types.source_check = { source_run = run; source_info = info }
