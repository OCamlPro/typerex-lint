let set table =
  let dom_table = (Tyxml_js.To_dom.of_table table) in
  dom_table##classList##add (Js.string "ocp-data-table");
  dom_table##classList##add (Js.string "stripe");
  dom_table##classList##add (Js.string "hover");
  dom_table##classList##add (Js.string "row-border");
  dom_table##classList##add (Js.string "nowrap")
