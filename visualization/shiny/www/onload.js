

shinyjs.ping_display_size = function()
{
    var rv = {width: document.body.clientWidth,
                height: document.body.clientHeight};
    
    Shiny.setInputValue('display_size', rv);
}