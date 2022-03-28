$(document).on("shiny:busy", function() {
  var inputs = document.getElementsByTagName("button");
  console.log(inputs);
for (var i = 0; i < inputs.length; i++) {
inputs[i].disabled = true;
}
});

$(document).on("shiny:idle", function() {
  var inputs = document.getElementsByTagName("button");
  console.log(inputs);
for (var i = 0; i < inputs.length; i++) {
inputs[i].disabled = false;
}
});

var dimension = [0, 0];
$(document).on("shiny:connected", function(e) {
	dimension[0] = window.innerWidth;
	dimension[1] = window.innerHeight;
	Shiny.onInputChange("dimension", dimension);
	});
$(window).resize(function(e) {
	dimension[0] = window.innerWidth;
	dimension[1] = window.innerHeight;
	Shiny.onInputChange("dimension", dimension);
});

$('[data-toggle="popover"]').tooltip({
    trigger : 'hover'
})    
