open Check_types
open Configuration
open Info

let info = {
  name = "Test template";
  details = "Long details";
  cat = Code;
}

let run config reports cmt =
  (* Iterator on typedtree *)
  let module IteratorArg =
    (struct
      include TypedtreeIter.DefaultIteratorArgument
      let enter_pattern p =
        let open Typedtree in
        match p.pat_desc with
        | Tpat_var (id, _) ->
          let id_str = Ident.name id in
          let id_len = String.length id_str in
          let min_len = config.min_identifier_len in
          let max_len = config.max_identifier_len in
          if id_len < min_len then
          let msg =
            Printf.sprintf
              "%S is too short: it should be at least of size '%d'."
              id_str
              min_len in
          Reports.add (Reports.warning p.pat_loc info msg) reports;
          if id_len > max_len then
            let msg =
              Printf.sprintf "%S is too long: it should not exceed '%d'.\n%!"
                id_str
                max_len in
            Reports.add (Reports.warning p.pat_loc info msg) reports
        | _ -> ()
    end)  in

  let iterator = (module IteratorArg : TypedtreeIter.IteratorArgument) in
  iter_cmt iterator cmt

let check : Check_types.cmt_check = { cmt_run = run; cmt_info = info }
