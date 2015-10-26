function focus() {
    document.getElementById("readline").focus();
}

function jrun() {
    var expr = document.getElementById("readline").value ;
    var checked = document.getElementById("lambda").checked ;
    document.getElementById("term").innerHTML +=
        expr + "<br />";
    document.getElementById("readline").value = "";
    document.getElementById("term").innerHTML +=
        run(checked, expr) + "phalang15> ";
    focus();
}

