let doc = Dom_html.document

open Yojson.Basic.Util

(* **** Faire ca a partir db **** *)
let tmp_entries_of_json_db json_db =
  (* ptt utiliser record pour retour (record du db type si possible) *)
  []
(* ****************************** *)
    
let onload _ =
  let (str_js: Js.js_string Js.t) = Js.Unsafe.variable "json" (* lint_web.var_json_name *) in
  let json = Yojson.Basic.from_string (Js.to_string str_js) in
  let entries = tmp_entries_of_json_db json in
  let div = Dom_html.createDiv doc in
  List.iter begin fun _ (****) ->

		  ()

  end entries;
  Dom.appendChild doc##body div;
  Js._false

let () =
  Dom_html.window##onload <- Dom_html.handler onload
			      
