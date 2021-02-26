

test.modal <- function(session)
{
    print("About to test modal")
    
    modal = modalDialog('this is a test modal',
                        title='test title')
    showModal(ui=modal,
              session=session)
    
    print('AFTER modal')
}