function jrun() {
    var expr = document.getElementById("readline").value ;
    document.getElementById("readline").value = "";
    document.getElementById("term").innerHTML +=
        run(false, expr) + "phalang15> ";
}

