

shinyjs.download_plotly = function(params)
{
    var id = params['id'];
    var filename = params['filename'];
    var elem = document.getElementById(id);
    
    var width = elem.clientWidth;
    var height = elem.clientHeight;
    
    Plotly.downloadImage(elem,
                         {format: 'png', 
                          width: width, 
                          height: height,
                          filename: filename
                         })
}