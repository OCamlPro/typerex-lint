function hide_all() {
    var contents = document.getElementsByClassName("content");
    for (var i = 0; i < contents.length; ++i) {
        var item = contents[i];
        item.style.display = "none";
    }
}

function hide_all_menu(){
    var hl_li = document.getElementsByClassName("li_selected");
    for (var i = 0; i < hl_li.length; ++i) {
        var item = hl_li[i];
        item.className = "";
    }
}

function show(id){
    console.log("looking for " + id);
    var div = document.getElementById(id);
    div.style.display = "block";
}

function show_menu(id){
    console.log("looking for " + id);
    var li = document.getElementById(id);
    li.className = "li_selected"
}

function handler(id){
    var div_id = id + "_div";
    window.location.hash = "#" + div_id;
}

function handler_warn(id){
    window.location.hash = "#" + id;
}

function handler_search(id){
    var anchor = id;
    var res = anchor.split("__");
    var linter_name = res[1];
    var div_id = linter_name + "_div";
    var li_id = linter_name + "_li";
    window.location.hash = "#" + anchor;
}

function searching() {
  var input, filter, table, tr, tdpname, tdlname, tdname, i;
  input = document.getElementById("search_box");
  filter = input.value.toUpperCase();
  table = document.getElementById("search_tab");
  tr = table.getElementsByTagName("tr");

  for (i = 0; i < tr.length; i++) {
      tdpname = tr[i].getElementsByTagName("td")[0];
      tdlname = tr[i].getElementsByTagName("td")[1];
      tdname = tr[i].getElementsByTagName("td")[2];
    if (tdpname && tdlname && tdname) {
      if (tdpname.innerHTML.toUpperCase().indexOf(filter) > -1 ||
          tdlname.innerHTML.toUpperCase().indexOf(filter) > -1 ||
          tdname.innerHTML.toUpperCase().indexOf(filter) > -1) {
          tr[i].style.display = "";
      } else {
          tr[i].style.display = "none";
      }
    }
  }
}

function select_first (){
    hide_all();
    hide_all_menu();
    var anchor = window.location.hash.substring(1);
    console.log(anchor);
    if (anchor == "") {
      var id = "home";
      var div_id = id + "_div";
      var li_id = id + "_li";
      show(div_id);
      show_menu(li_id)
    }
    else {
        if (anchor.endsWith("_div")) {
            var div_id = anchor;
            var li_id = anchor.replace("_div","_li");
            show(div_id);
            show_menu(li_id);
        }
        else {
            if (anchor.endsWith("_warn")) {
                var res = anchor.split("__");
                var linter_name = res[1];
                var div_id = linter_name + "_div";
                var li_id = linter_name + "_li";
                show(div_id);
                show_menu(li_id);
            }
        }
    }
}

select_first();

window.addEventListener('popstate', function (e) {
    var state = e.state;
    hide_all();
    hide_all_menu();
    var anchor = window.location.hash.substring(1);
    console.log(anchor);
    if (anchor == "") {
      var id = "home";
      var div_id = id + "_div";
      var li_id = id + "_li";
      show(div_id);
      show_menu(li_id)
    }
    else {
        if (anchor.endsWith("_div")) {
            var div_id = anchor;
            var li_id = anchor.replace("_div","_li");
            show(div_id);
            show_menu(li_id);
        }
        else {
            if (anchor.endsWith("_warn")) {
                var res = anchor.split("__");
                var linter_name = res[1];
                var div_id = linter_name + "_div";
                var li_id = linter_name + "_li";
                show(div_id);
                show_menu(li_id);
            }
        }
    }
});
