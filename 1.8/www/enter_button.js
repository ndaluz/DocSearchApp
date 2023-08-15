$(document).keyup(function(event) {
    if ($("#input_keys").is(":focus") && (event.key == "Enter")) {
        $("#run_search").click();
    }
});