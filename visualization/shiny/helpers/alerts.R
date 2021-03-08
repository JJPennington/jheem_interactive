

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
    link.url = make.link.url(link)
    
    msg = tags$div("We have created a link to these projections. This link will remain active for one year.",
                   tags$br(),
                   tags$br(),
                   tags$b(link.url),
                   tags$div(style='height: 50%', tags$br()),
                   tags$button('Copy Link to Clipboard',
                               id = 'copy_link_button',
                                     onclick=paste0('copyToClipboard("', link.url,'", "copy_link_button", "copied_div")')),
                   tags$div(id="copied_div",
                            style='display: none; font-style: italic;',
                            "The link has been copied to the clipboard")
                   )
    
    sendSweetAlert(session=session,
                   title='Link Created',
                   text=msg,
                   type='success',
                   btn_labels='Close',#c('Copy Link to Clipboard', 'Close'),
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