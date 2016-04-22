
let lint all mls mlis asts_ml asts_mli cmts =
  (* Itering on all files in your project *)
  Plugin.iter_plugins (fun plugin checks ->
      Plugin.LintMap.iter (fun cname runs ->
          List.iter (function
              | Input.InAll main -> main all
              | _ -> ()) runs) checks);

  (* Itering on ml sources *)
  List.iter (fun input ->
      Plugin.iter_plugins (fun plugin checks ->
          Plugin.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InMl main -> main input
                  | _ -> ()) runs) checks))
    mls;

  (* Itering on mli sources *)
  List.iter (fun input ->
      Plugin.iter_plugins (fun plugin checks ->
          Plugin.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InMli main -> main input
                  | _ -> ()) runs) checks))
    mlis;

  (* Itering on Parsetree.structure *)
  List.iter (fun input ->
      Plugin.iter_plugins (fun plugin checks ->
          Plugin.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InStruct main -> main (Lazy.force input)
                  | _ -> ()) runs) checks))
    asts_ml;

  (* Itering on Parsetree.signature *)
  List.iter (fun input ->
      Plugin.iter_plugins (fun plugin checks ->
          Plugin.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InInterf main -> main (Lazy.force input)
                  | _ -> ()) runs) checks))
    asts_mli;

  (* Itering on cmts *)
  List.iter (fun input ->
      Plugin.iter_plugins (fun plugin checks ->
          Plugin.LintMap.iter (fun cname runs ->
              List.iter (function
                  | Input.InCmt main -> main (Lazy.force input)
                  | _ -> ()) runs) checks))
    cmts;

  (* TODO XX do not forget InTop case *)

  (* TO REMOVE : just for testing output *)
  Plugin.iter_plugins (fun plugin checks ->
      let module P = (val plugin : Plugin_types.PLUGIN) in

      Warning.iter
        (fun warning -> Warning.print Format.err_formatter warning)
        P.warnings)
