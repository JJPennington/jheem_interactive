

show.error.message <- function(session, title, message)
{
    sendSweetAlert(session=session,
                   title=title,
                   text=message,
                   type='error',
                   btn_labels='OK',
                   html=T)
}

show.warning.message <- function(session, title, message)
{
    sendSweetAlert(session=session,
                   title=title,
                   text=message,
                   type='warning',
                   btn_labels='OK',
                   html=T)
}


show.link.message <- function(session,
                              link)
{
    msg = tags$div("We have created a link to this content. This link will remain active for one year.",
                   tags$br(),
                   tags$br(),
                   make.link.url(link))
    
    sendSweetAlert(session=session,
                   title='Link Created',
                   text=msg,
                   type='success',
                   btn_labels='Close',
                   html=T)
}




log.error <- function(msg, error=NULL)
{
    print(msg)
}