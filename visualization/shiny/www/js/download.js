

Shinyjs.download_plotly = function(params)
{
    alert('download');
    var id = params['id'];
    var filename = paramets['filename'];
    var elem = document.getElementById(id);
    
    var width = elem.clientWidth;
    var height = elem.clientHeight;
    
    Plotly.downloadImage(id,
                         {format: 'png', 
                          width: width, 
                          height: height,
                          filename: filename
                         })
}