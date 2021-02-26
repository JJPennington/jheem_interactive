

start.progress.bar <- function(session,
                               id='main_progress',
                               initial.value=0,
                               total=1,
                               title=NULL)
{
    print('set up sweet progress bar')
    progressBar(session=session,
                       id=id,
                       value=initial.value,
                       total = total,
                       title=title,
                       display_pct = T)
}

update.progress.bar <- function(session,
                                id='main_progress',
                                value)
{
    print('update progress bar')
    updateProgressBar(session=session,
                      id=id,
                      value=value)
}

end.progress.bar <- function(session)
{
    closeSweetAlert(session)
}

progress.test <- function(session)
{
    start.progress.bar(session, total=100)
    for (i in 1:50) {
        Sys.sleep(0.1)
        update.progress.bar(
            session = session,
            value = i*2
        )
    }
    end.progress.bar(session)
}



create.modal.progress <- function(session,
                                  id='progress',
                                  title,
                                  value,
                                  total,
                                  display_pct=T)
{
    progress.bar = progressBar(id,
                               value=value,
                               total=total,
                               display_pct = display_pct)
    
    content = basicPage(
        tags$div(class='modal_header', title),
        tags$div(class='modal_content',
                 progress.bar,
                 uiOutput(paste0(id, '_text')))
    )
    
    modal = mo
}