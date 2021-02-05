

CONTACT.CONTENT = 
  fluidPage(
    fluidRow(verticalSpacer(40)),
    fluidRow(
    #  ,
      
      column(width=1),
      box(
        width=10, 
        title=tags$div(icon('envelope'), "Contact Form"),
        collapsible=F,
        collapsed=F,
        status="primary", 
        solidHeader=TRUE,
        
        tableRow(
          inner.padding='25px',
          column(
            width=12,
            fluidRow(
              # tableRow(
              # vertical.align='top',
              # inner.padding='25px',
              textInput(
                inputId='feedback_name', 
                label='Your name') ),
            fluidRow(
              textInput(
                inputId='feedback_email', 
                label='Your email') )
          ),
          column(
            width=12,
            textAreaInput(
              inputId='feedback_contents', 
              label='Your message',
              height='250px',
              width='375px',
              # cols=80,
              # rows=6,
              )
          )
        ),
        tableRow(
          inner.padding='25px',
          actionButton(
            inputId='feedback_submit',
            label='Submit')
        )
      )

      
  ))

