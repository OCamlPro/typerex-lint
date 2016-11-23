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
    div.style.display = "block"
}

function show_menu(id){
    console.log("looking for " + id);
    var li = document.getElementById(id);
    li.className = "li_selected"
}

function handler(id){
    console.log("handler " + id);
    var div_id = id + "_div";
    var li_id = id + "_li";
    hide_all();
    hide_all_menu();
    show(div_id);
    show_menu(li_id);
}

function select_first (){
    hide_all();
    hide_all_menu();
    var id = "plugin_parsing";
    var div_id = id + "_div";
    var li_id = id + "_li";
    show(div_id);
    show_menu(li_id)
}

select_first();
