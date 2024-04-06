$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  var gid = $el.data("gid");
  $($("#data_explorer_tabset a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: long,
    gid: gid,
    nonce: Math.random()
  });
});