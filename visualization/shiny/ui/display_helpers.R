
##-- CREATE THE DISPLAY PANEL --##
##------------------------------##

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
                uiOutput(outputId = paste0('figure_', suffix))
#                tags$div(id=paste0('plot_holder_', suffix), 'temp text')
#                HTML('This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.<br/>This is a figure.')
            ),
            tabPanel(
                title="Table",
                uiOutput(outputId = paste0('table_', suffix))
                #tags$div(id=paste0('table_holder_', suffix), 'table placeholder')
#                HTML('This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.<br/>This is a table.')
            )

#            tabPanel(
#                title="(Hide)",
#                ''
#            ),
#            tabPanel(
#                title="Run",
#                ''
#            ),
            # navbarMenu(
            #     title="Share",
            #     menuName = paste0("share_", suffix),
            #     # Error when using icon:
            #     # Warning: Error in : $ operator is invalid for atomic vectors
            #     # icon='share-alt',
            #     tabPanel(
            #         title=tags$a("Download figure", href='test'),
            #         id='test1'
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

##-----------------------------##
##-- MANIPULATE THE CONTENTS --##
##-----------------------------##

set.display <- function(input, output, suffix, plot.and.table)
{
    set.plot(output, suffix, plot.and.table$plot)
    set.table(output, suffix, plot.and.table$table)
    set.share.enabled(input, suffix, T)
}

clear.display <- function(input, output, suffix)
{
    clear.plot(output, input, suffix)
    clear.table(output, input, suffix)
    set.share.enabled(input, suffix, F)
}


set.plot <- function(output,
                     suffix,
                     plot)
{
    figure.id = paste0('figure_', suffix)
    output[[figure.id]] = renderUI(tags$div(
        plot
    ))
}

set.table <- function(output,
                      suffix,
                      plot)
{
    table.id = paste0('table_', suffix)
 #   output[[holder.id]] = renderText(paste0("New One: ", Sys.time()))
}

clear.plot <- function(output,
                       input,
                       suffix)
{
    figure.id = paste0('figure_', suffix)
    output[[figure.id]] = renderUI(
        tags$div(class='empty_message',
                 tags$div(style='height:10vh'),
                 "Use the controls to the left to generate a figure for ",
                 msa.names(get.selected.location(input, suffix)))
    )
}

clear.table <- function(output,
                        input,
                        suffix)
{
    table.id = paste0('table_', suffix)
    output[[table.id]] = renderUI(
        tags$div(class='empty_message',
                 tags$div(style='height:10vh'),
                 "Use the controls to the left to generate a table for ",
                 msa.names(get.selected.location(input, suffix)))
    )
}

set.share.enabled <- function(input,
                               suffix,
                               enabled)
{
    if (enabled)
    {
        shinyjs::enable(paste0('redraw_', suffix))
    }
    else
    {
        shinyjs::disable(paste0('redraw_', suffix))
    }
}

