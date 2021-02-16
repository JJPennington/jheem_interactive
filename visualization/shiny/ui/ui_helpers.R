
inline.select.input <- function(inputId,
                                label,
                                choices,
                                width=NULL,
                                selectize=F)
{
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
                            selectize=selectize)
                )
        ))
    )
}