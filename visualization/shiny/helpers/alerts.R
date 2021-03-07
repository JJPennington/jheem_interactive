

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

show.success.message <- function(session, title, message)
{
    sendSweetAlert(session=session,
                   title=title,
                   text=message,
                   type='success',
                   btn_labels='Close',
                   html=T)
}

show.link.message <- function(session,
                              link)
{
    msg = tags$div("We have created a link to these projections. This link will remain active for one year.",
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




log.error <- function(error, msg=NULL)
{
    print("-------------------------")
    print("--------- ERROR ---------")
    print(error)
    if (!is.null(msg))
    {
        print("***")
        print(msg)
    }
    print("-------------------------")
}