
##-----------------------------------##
##-- CONSTRUCTOR FOR THE PANEL GUI --##
##-----------------------------------##

create.plot.control.panel <- function(suffix)
{
    tags$div(
        class='controls',
        
        
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
                     title='Split Outcomes by Subgroup',
                     content="Make a separate panel for each combination of the selected attributes. (Note: clicking more than two attributes is going to make a LOT of panels)",
                     placement='left'),
        
        
        tags$div(id=paste0('show_change_panel_', suffix),
            checkboxInput(inputId = paste0('show_change_', suffix),
                          label = tags$b('Calculate the Change in Outcomes'),
                          value=T),
            
            conditionalPanel(
                condition = paste0("input.show_change_", suffix),
                fluidRow(class='from_to_holder',
                    column(width=6, 
                           'from',
                           selectInput(inputId = paste0('change_from_', suffix),
                                       label = NULL,
                                       choices = YEAR.OPTIONS$values[YEAR.OPTIONS$values<max(YEAR.OPTIONS$values)],
                                       selected = min(YEAR.OPTIONS$values[1]))),
                    column(width=6,
                           'to',
                           selectInput(inputId = paste0('change_to_', suffix),
                                        label = NULL,
                                        choices = YEAR.OPTIONS$values[-1],
                                        selected = max(YEAR.OPTIONS$values)))
                ) #end fluidRow
            ) #end conditionalPanel
        ),
        make.popover(paste0('show_change_panel_', suffix),
                     title='Calculate a Change In Outcomes',
                     content="The tigure and table will display the change in each outcome over this time period.",
                     placement='left'),

        
        radioButtons(inputId = paste0('plot_format_', suffix),
                     label='What to Show for Projections:',
                     choiceNames=PLOT.FORMAT.OPTIONS$names,
                     choiceValues=PLOT.FORMAT.OPTIONS$values,
                     selected=PLOT.FORMAT.OPTIONS$values[1]),
        make.popover(paste0('plot_format_', suffix),
                     title='What to Show in the Plot',
                     content="Each figure and table is a synthesis of 80 individual simulations. You can either show the mean and credible interval across these simulations, or you can plot a line for each individual simulation.",
                     placement='left'),
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
    NULL
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