open Asttypes
open Parsetree
open Ast_mapper
open Check_types
open Configuration
open Info

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
        let id_str =  ident.txt in
        let id_loc = ident.loc in
        let id_len = String.length id_str in
        let min_len = config.min_identifier_len in
        let max_len = config.max_identifier_len in
        if id_len < min_len then
          Format.eprintf  "%a\n  %S is too short: it should be at least of size '%d'.\n%!"
            Location.print_loc id_loc
          id_str
          min_len;
        if id_len > max_len then
          Format.eprintf "%a\n  %S is too long: it should not exceed '%d'.\n%!"
            Location.print_loc id_loc
            id_str
            max_len
      | _ -> () end;
      pat
  }

let run config reports source =
  ignore (check_source (mapper config reports) source);
  reports

let check : Check_types.source_check = { source_run = run; source_info = info }
