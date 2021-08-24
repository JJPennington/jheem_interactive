// First we get the viewport height
let total_height = window.outerHeight;//document.documentElement.clientHeight;//document.body.clientHeight; //window.innerHeight;

// Then we set the value in the --total_height custom property to the root of the document
document.documentElement.style.setProperty('--total_height', `${total_height}px`);

// We listen to the resize event
window.addEventListener('resize', () => {
  // We execute the same script as before
    let total_height = document.documentElement.clientHeight;//document.body.clientHeight; //window.innerHeight;
    alert(window.innerHeight);
    document.documentElement.style.setProperty('--total_height', `${total_height}px`);
});