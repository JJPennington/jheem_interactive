
make.accordion.button <- function(id,
                                  show.ids=NULL,
                                  hide.ids=id,
                                  remove.class.ids=NULL,
                                  remove.classes=NULL,
                                  add.class.ids=NULL,
                                  add.classes=NULL,
                                  shiny.ids=NULL,
                                  shiny.values=NULL,
                                  direction=c('right','left')[1],
                                  right.offset=NULL,
                                  left.offset=NULL,
                                  inputId = NULL,
                                  input.value = NULL,
                                  height='30px',
                                  visible=T)
{
    #-- Set up the Chevron --#
    if (direction=='right')
        chevron.class = 'chevron_right'
    else
        chevron.class = 'chevron_left'
    
 #   chevron.class='chevron'
    
    #-- Set up the style attribute --#
    style = paste0("font-size: ", height, "; ")
    if (!is.null(right.offset))
        style = paste0(style, "right: ", right.offset, "; ")
    if (!is.null(left.offset))
        style = paste0(style, "left: ", left.offset, "; ")
    if (!visible)
        style = paste0(style, "display: none; ")
    
    #-- Set up the data attributes --#
    data.attributes = ''
    
    if (!is.null(show.ids))
        data.attributes = paste0(data.attributes,
                                 "data-show_targets='", paste0(show.ids, collapse=';'), "' ")
    
    if (!is.null(hide.ids))
        data.attributes = paste0(data.attributes,
                                 "data-hide_targets='", paste0(hide.ids, collapse=';'), "' ")
    
    if (!is.null(remove.class.ids))
    {
        if (!is.null(remove.classes) && length(remove.classes)==1)
            remove.classes = rep(remove.classes, length(remove.class.ids))
        
        if (is.null(remove.classes) || length(remove.classes) != length(remove.class.ids))
            stop("remove.classes must have the same length as remove.class.ids")
        
        data.attributes = paste0(data.attributes,
                                 "data-remove_class_targets='", paste0(remove.class.ids, collapse=';'), "' ",
                                 "data-remove_classes='", paste0(remove.classes, collapse=';'), "' ")
    }
  
    if (!is.null(add.class.ids))
    {
        if (!is.null(add.classes) && length(add.classes)==1)
            add.classes = rep(add.classes, length(add.class.ids))
        
        if (is.null(add.classes) || length(add.classes) != length(add.class.ids))
            stop("add.classes must have the same length as add.class.ids")
        
        data.attributes = paste0(data.attributes,
                                 "data-add_class_targets='", paste0(add.class.ids, collapse=';'), "' ",
                                 "data-add_classes='", paste0(add.classes, collapse=';'), "' ")
    }
    
    if (!is.null(shiny.ids))
    {
        if (!is.null(shiny.values) && length(shiny.values)==1)
            shiny.values = rep(shiny.values, length(shiny.ids))
        
        if (is.null(shiny.values) || length(shiny.values) != length(shiny.ids))
            stop("shiny.values must have the same length as shiny.ids")
        
        data.attributes = paste0(data.attributes,
                                 "data-shiny_targets='", paste0(shiny.ids, collapse=';'), "' ",
                                 "data-shiny_values='", paste0(shiny.values, collapse=';'), "' ")
    }
    
    #-- Make the HTML
    HTML(paste0("<div class='accordion_button' id='", id, "' ",
                "style='", style, "' ",
                data.attributes,
                ">",
                     "<div class='", chevron.class, "'></div>",
                "</div>"
                ))
}