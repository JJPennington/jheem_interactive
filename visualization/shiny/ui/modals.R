library('shinyalert')

# Modal dialogue boxes ####
showMessageModal <- function(message, title=NULL) {
  showModal(
    modalDialog(
      title=title,
      footer=modalButton("Dismiss"),
      size=c("m", "s", "l"),
      easyClose=FALSE,
      fade=TRUE,
      verticalSpacer(40),
      # HTML(paste0('<br/><br/>', message, '<br/><br/>'))
      message
  ))
}

showValidationModal <- function(
  message, 
  title='Invalid selection(s)',
  type=c('info', 'success', 'warning', 'error')[4]  # 3 = error
) {
  # TODO 
}

'Returns T if clicking "confirm", else F.'
showYesNoValidationModal <- function(
  message, 
  callback,
  # title=NULL,
  type=c('info', 'success', 'warning', 'error')[3]  # 3 = warning
  # type='input'
  ) {
  # showModal(
    shinyalert(
      # title=title,
      type=type,
      closeOnEsc=TRUE,
      closeOnClickOutside=FALSE,
      showCancelButton=TRUE,
      showConfirmButton=TRUE,
      confirmButtonText='Yes',
      cancelButtonText='No',
      # html=TRUE,
      text=message,
      callbackR=callback
      # Warning: Error in : Cannot use 'input' type and HTML together. You must supply your own Shiny inputs when using HTML.
      # text=tags$div(
      #   verticalSpacer(40),
      #   HTML(paste0('<br/><br/>', message, '<br/><br/>'))
      # )
    )
  # )
}
