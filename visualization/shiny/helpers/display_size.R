
# Code to (roughly) track the size of the display panel using javascript

LEFT.PANEL.SIZE = c(prerun=250,
                    custom=500)
RIGHT.PANEL.SIZE = c(prerun=250,
                     custom=250)
HEIGHT.OFFSET = c(prerun=180,
                  custom=180) #to allow for the bottom panel if collapsed

get.display.size <- function(input, suffix)
{
    size = input$display_size
    
    size$width = size$width -
        as.numeric(input[[paste0('left_width_', suffix)]]) -
        as.numeric(input[[paste0('right_width_', suffix)]])
    size$height = size$height - HEIGHT.OFFSET[suffix]
    
    size
}

# window_size.js updates the input on a resize
add.display.size.observers <- function(session, input)
{
}