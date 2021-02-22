

shinyjs.ping_display_size = function()
{
    var rv = {width: document.body.clientWidth,
                height: document.body.clientHeight};
    
    Shiny.setInputValue('display_size', rv);
}


shinyjs.set_input_value = function(params)
{
    Shiny.setInputValue(params.name, params.value);
}