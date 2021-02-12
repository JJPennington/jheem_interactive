
var acc = document.getElementsByClassName("accordion_button");
var i;

for (i = 0; i < acc.length; i++) {
  acc[i].addEventListener("click", function() {
//    this.classList.toggle("active");
    var target_ids = this.getAttribute('data-targets')
    var panel = this.nextElementSibling;
    if (panel.style.maxHeight) {
      panel.style.maxHeight = null;
    } else {
      panel.style.maxHeight = panel.scrollHeight + "px";
    }
  });
}