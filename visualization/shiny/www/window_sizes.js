
get_display_size = function()
{
    var elems = document.getElementsByClassName('tab-content')
    var width=0;
    var height=0;
    
    for (var i=0; i<elems.length; i++)
    {
        var elem = elems[i];
        
        if (elem.clientWidth > width)
            width = elem.clientWidth;
        if (elem.clientHeight > height)
            height = elem.clientHeight;
    }
    
    var rv = {width: document.body.clientWidth,
                height: height};
    
    Shiny.setInputValue('display_size', rv);
} 

window.addEventListener('resize', () => {
    var rv = {width: document.body.clientWidth,
                height: document.body.clientHeight};
    
    Shiny.setInputValue('display_size', rv);
});