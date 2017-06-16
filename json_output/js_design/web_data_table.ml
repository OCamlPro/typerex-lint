let set table =
  let dom_table = (Tyxml_js.To_dom.of_table table) in
  dom_table##classList##add (Js.string "ocp-data-table");
  dom_table##classList##add (Js.string "display")
