
##-----------------------------------##
##-- CONSTRUCTOR FOR THE PANEL GUI --##
##-----------------------------------##

create.plot.control.panel <- function(suffix)
{
    tags$div(
        class='controls controls_narrow',
        
        
        checkboxGroupInput(inputId = paste0('outcomes_', suffix),
                           label = "Outcomes",
                           choiceValues = OUTCOME.OPTIONS$values,
                           choiceNames = OUTCOME.OPTIONS$names,
                           selected = OUTCOME.OPTIONS$values[1:2]),
        make.popover(paste0('outcomes_', suffix),
                     title='Epidemiological Outcomes to Display',
                     content="Select the epidemiological outcomes for which to display projections.",
                     placement='left'),
        
        checkboxGroupInput(inputId = paste0('facet_by_', suffix),
                           label = "Separate Projections By:",
                           choiceValues = DIMENSION.OPTIONS.1$values,
                           choiceNames = DIMENSION.OPTIONS.1$names,
                           selected = c()),
        make.popover(paste0('facet_by_', suffix),
                     title='Make Separate Panels by Subgroup',
                     content="Make a separate panel for each combination of the selected attributes. (Note: clicking more than two attributes is going to make a LOT of panels)",
                     placement='left'),
        
        
        tags$div(id=paste0('show_change_panel_', suffix),
                 tags$b('Calculate the Change in Outcomes'),
                 
                 tags$table(class='table_select',
                     tags$tr(
                         tags$td("from:"),
                         tags$td(selectInput(inputId = paste0('change_from_', suffix),
                                             label = NULL,
                                             width = '120px',
                                             choices = YEAR.OPTIONS$values[YEAR.OPTIONS$values<max(YEAR.OPTIONS$values)],
                                             selected = min(YEAR.OPTIONS$values[1]),
                                             selectize = F))
                     ),
                     tags$tr(
                         tags$td("to:"),
                         tags$td(selectInput(inputId = paste0('change_to_', suffix),
                                             label = NULL,
                                             width='120px',
                                             choices = YEAR.OPTIONS$values[-1],
                                             selected = max(YEAR.OPTIONS$values),
                                             selectize = F))
                     )
                 ), #</table>
                 
                 tags$div(class='squish_up',
                     checkboxInput(
                         inputId = paste0('show_change_', suffix),
                         label='Label this change on the figure',
                         value=T
                     ) #</div>
                 )
             ), #</div>
         
        make.popover(paste0('show_change_panel_', suffix),
                     title='Calculate a Change In Outcomes',
                     content="The tigure and table will display the change in each outcome over this time period.",
                     placement='left'),

        tags$div(style='height: 20px;'),
        
        tags$div(style='margin-right:-5px; margin-left:-5px',
        box(title='Advanced Options',
            status = 'info',
            solidHeader = T,
            width=12,
            collapsible = T,
            collapsed=T,
            
            #-- Plot Format --#
            radioButtons(inputId = paste0('plot_format_', suffix),
                         label='What to Show for Projections:',
                         choiceNames=PLOT.FORMAT.OPTIONS$names,
                         choiceValues=PLOT.FORMAT.OPTIONS$values,
                         selected=PLOT.FORMAT.OPTIONS$values[1]),
            make.popover(paste0('plot_format_', suffix),
                         title='What to Show in the Plot',
                         content="Each figure and table is a synthesis of 80 individual simulations. You can either show the mean and credible interval across these simulations, or you can plot a line for each individual simulation.",
                         placement='left'),
            
            #-- Split By --#
            checkboxGroupInput(inputId = paste0('split_by_', suffix),
                               label = "Plot a Separate Line for Each:",
                               choiceValues = DIMENSION.OPTIONS.1$values,
                               choiceNames = DIMENSION.OPTIONS.1$names,
                               selected = c()),
            make.popover(paste0('split_by_', suffix),
                         title='Make Separate Lines by Subgroup',
                         content="Within each panel, plot a separate line for each combination of the selected attributes. (Note: clicking more than one attribute is going to make a LOT of lines)",
                         placement='left'),
        
        ) #</box>
        ) #</div>
    )
    
    
}

##-----------------------------------------------##
##-- EVENT HANDLERS FOR UPDATING PLOT CONTROLS --##
##-----------------------------------------------##

add.control.event.handlers <- function(session, input, output, cache, suffix)
{   
    change.from.id = paste0('change_from_', suffix)
    change.to.id = paste0('change_to_', suffix)
    add.year.range.dropdown.handler(session, input,
                                    change.from.id, change.to.id,
                                    min.delta=1)
}


add.year.range.dropdown.handler <- function(session, input,
                                            from.id, to.id,
                                            min.delta,
                                            years = YEAR.OPTIONS$values)
{
    observeEvent(input[[from.id]], {
        prev.to = input[[to.id]]
        min.year = as.numeric(input[[from.id]]) + min.delta
        new.choices = years[years >= min.year]
        updateSelectInput(session,
                          inputId = to.id,
                          choices = new.choices,
                          selected = max(prev.to, min(new.choices))
        )
    })
}

##-------------##
##-- GETTERS --##
##-------------##

# Hard-coded for now
get.version <- function(input, suffix)
{
    '1.0'
}

get.selected.location <- function(input, suffix)
{
    input[[paste0("location_", suffix)]]
}

get.selected.years <- function(input, suffix)
{
    2010:2030
}

get.selected.outcomes <- function(input, suffix)
{
    input[[paste0('outcomes_', suffix)]]
}

get.selected.facet.by <- function(input, suffix)
{
    input[[paste0('facet_by_', suffix)]]
}

get.selected.split.by <- function(input, suffix)
{
    input[[paste0('split_by_', suffix)]]
}

get.selected.dimension.subsets <- function(input, suffix)
{
    ALL.DIMENSION.VALUES
}

get.selected.plot.format <- function(input, suffix)
{
    input[[paste0('plot_format_', suffix)]]
}

get.selected.interval.coverage <- function(input, suffix)
{
    0.95
}

get.selected.show.change <- function(input, suffix)
{
    input[[paste0('show_change_', suffix)]]
}

get.selected.change.years <- function(input, suffix)
{
    as.numeric(c(input[[paste0('change_from_', suffix)]],
                 input[[paste0('change_to_', suffix)]]))
}