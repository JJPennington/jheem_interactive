
create.display.panel <- function(suffix)
{
    # Output ####
    fluidRow(
        # column(
        #   width=page.width,
        
        # div(
        #   class="sticky_footer", 
        #   p("test footer")),
        
        navbarPage(
            id=paste0('nav_', suffix),
            title=NULL,
            # collapsible=TRUE,
            
            # header=tags$div(
            #   'header'
            # ),
            
            # footer=tags$div(
            #   'footer'
            # ),
            
            # Menu
            tabPanel(
                title="Figure",
                HTML('This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.')
            ),
            tabPanel(
                title="Table",
                HTML('This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.')
            )
            
            # TODO: re-add
            # tabPanel(
            #     title="(Hide)",
            #     ''
            # ),
            # tabPanel(
            #     title="Run",
            #     ''
            # ),
            # navbarMenu(
            #     title="Share",
            #     # Error when using icon:
            #     # Warning: Error in : $ operator is invalid for atomic vectors
            #     # icon='share-alt',
            #     tabPanel(
            #         title="Download figure",
            #         # 'hello figure'
            #     ),
            #     tabPanel(
            #         title="Download table",
            #         # 'hello table'
            #     ),
            #     tabPanel(
            #         title="Share link",
            #         # 'hello share link'
            #     )
            # )  # </navbarMenu>
            
        )  # </navbarPage>    
    ) #</fluidRow>
}