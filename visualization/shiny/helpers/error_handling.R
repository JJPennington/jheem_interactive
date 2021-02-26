

show.error.message <- function(title, message)
{
    modal = modalDialog(title,
                        tags$div(message))
}