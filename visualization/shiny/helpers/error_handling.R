

show.error.message <- function(title, message)
{
    modal = modalDialog(title,
                        tags$div(message))
}




log.error <- function(msg, error=NULL)
{
    print(msg)
}