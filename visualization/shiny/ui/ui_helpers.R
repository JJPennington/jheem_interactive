
inline.select.input <- function(inputId,
                                label,
                                choices=NULL,
                                width=NULL,
                                selected=NULL,
                                choiceNames=NULL,
                                choiceValues=NULL,
                                selectize=F)
{
    if (!is.null(choiceNames) && !is.null(choiceValues))
    {
        choices = choiceValues
        names(choices) = choiceNames
    }
    
    tags$div(class='inline_select',
        tags$table(tags$tr(
            tags$td(
                tags$b(label)
                ),
            tags$td(
                selectInput(inputId=inputId,
                            label=NULL,
                            choices=choices,
                            width=width,
                            selected=selected,
                            selectize=selectize)
                )
        ))
    )
}


verticalSpacer <- function(height, unit='px')
{
    div(style=paste0("height: ", height, unit))
}

horizontalSpacer <- function(width, unit='px')
{
    div(style=paste0("width: ", width, unit))
}

tableRow <- function(...,
                     fill.width=T,
                     vertical.align='top',
                     inner.padding='25px')
{
    args = list(...)
    tds = lapply(1:length(args), function(i){
        elem = args[[i]]
        style = paste0("vertical-align: ", vertical.align)
        if (i>1)
            style = paste0(style, "; ",
                           "padding-left: ", inner.padding)
        
        tags$td(elem,
                style=style)
    })
    
    if (fill.width)
        tags$table(
            style='width: 100%',
            do.call(tags$tr, tds)
        )
    else
        tags$table(
            do.call(tags$tr, tds)
        )
}