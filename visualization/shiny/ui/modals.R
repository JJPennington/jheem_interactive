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
