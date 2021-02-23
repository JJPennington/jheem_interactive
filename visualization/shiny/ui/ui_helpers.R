
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