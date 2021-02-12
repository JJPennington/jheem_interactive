
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
        
        checkboxGroupInput(inputId = paste0('facet_by_', suffix),
                           label = "Separate Projections By:",
                           choiceValues = DIMENSION.OPTIONS.1$values,
                           choiceNames = DIMENSION.OPTIONS.1$names,
                           selected = c()),
        
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
        ), #end conditionalPanel

        
        radioButtons(inputId = paste0('plot_format_', suffix),
                     label='What to Show for Projections:',
                     choiceNames=PLOT.FORMAT.OPTIONS$names,
                     choiceValues=PLOT.FORMAT.OPTIONS$values,
                     selected=PLOT.FORMAT.OPTIONS$values[1])
    )
    
    
}

##-----------------------------------------------##
##-- EVENT HANDLERS FOR UPDATING PLOT CONTROLS --##
##-----------------------------------------------##

add.control.event.handlers <- function(session, input, output, cache, suffix)
{   
    change.from.id = paste0('change_from_', suffix)
    change.to.id = paste0('change_to_', suffix)
    observeEvent(input[[change.from.id]], {
        prev.to = input[[change.to.id]]
        new.choices = YEAR.OPTIONS$values[YEAR.OPTIONS$values > input[[change.from.id]] ]
        updateSelectInput(session,
                          inputId = change.to.id,
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