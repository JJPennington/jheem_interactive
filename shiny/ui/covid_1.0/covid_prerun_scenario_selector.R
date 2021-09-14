
##---------------##
##-- CONSTANTS --##
##---------------##

COVID.SCENARIOS = list(
    delayed.hiv.care = list(
        label='Prolonged Barriers to Care',
        description='Sexual transmission normalizes by July 4, 2021; HIV testing, viral suppression, and PrEP use do not normalize until Feb 4, 2022'
    ),
    
    base = list(
        label='Rapid Resumption of Care',
        description='Sexual transmission, HIV testing, viral suppression, and PrEP all normalize by July 4, 2021'
    )
)

COVID.PARAMETERS = list(
    list(
        name = 'sexual.transmission.reduction',
        id = 'covidsex',
        parameter.label = 'Sexual Transmission',
        effect.label = 'reduces',
        label = 'Reduction in Sexual Transmission',
        min=0,
        max=0.5
    ),
    list(
        name = 'suppression.reduction',
        id = 'covidsupp',
        parameter.label = 'Viral Suppression',
        effect.label = 'reduces',
        label = 'Reduction in Viral Suppression',
        min=0,
        max=0.4
    ),
    list(
        name = 'testing.reduction',
        id = 'covidtest',
        parameter.label = 'HIV Testing Rates',
        effect.label = 'reduces',
        label = 'Reduction in HIV Testing Rates',
        min=0,
        max=0.5
    ),
    list(
        name = 'prep.reduction',
        id = 'covidprep',
        parameter.label = 'PrEP Use',
        effect.label = 'reduces',
        label = 'Reduction in PrEP Use',
        min=0,
        max=0.3
    )
)
names(COVID.PARAMETERS) = sapply(COVID.PARAMETERS, function(pp){pp$name})

load('cache/COVID.PARAMETER.VALUES.Rdata')

##---------------------------##
##-- THE PANEL CONSTRUCTOR --##
##---------------------------##

create.covid.prerun.scenario.selector.panel <- function(suffix='prerun')
{
    scenario.choices = names(COVID.SCENARIOS)
    
    names(scenario.choices) = lapply(COVID.SCENARIOS, function(sc){
        sc$label
    })
    
    scenario.chooser = selectInput(inputId = paste0("covid_scenario_", suffix),
                                   label='What Timeline to Use:',
                                   choices = scenario.choices)
    
    scenario.descriptions = lapply(names(COVID.SCENARIOS), function(sc.name){
        conditionalPanel(
            condition = paste0("input.covid_scenario_", suffix," == '", sc.name, "'"),
            
            tags$div(
                style='margin-top:-1rem',
                tags$i(COVID.SCENARIOS[[sc.name]]$description))
        )
    })
    
    
    parameter.selectors = lapply(names(COVID.PARAMETERS), function(pp.name){
        pp = COVID.PARAMETERS[[pp.name]]
        tags$div(
            paste0(pp$label, ':'),
            sliderInput(
                inputId=paste0('covid_', pp.name, '_', suffix),
                label=NULL,#pp$label,
                min=100*pp$min,
                max=100*pp$max,
                value=100*c(pp$min, pp$max),
                post='%'
            ))
    })
    parameter.selector.panel = do.call(tags$div, parameter.selectors)
    
    sim.counter = tags$i(textOutput(outputId = paste0("covid_sim_counter_", suffix)))
    
    tags$div(
        wellPanel(
            id='covid_scenario_panel',
            style='padding: 5px',
            
            scenario.chooser,
            scenario.descriptions,
            
            make.popover('covid_scenario_panel',
                         title='Select How Long the Pandemic Affects HIV',
                         content="Select the scenario that specifies the time frame over which the COVID-19 pandemic has its effect on HIV",
                         placement='right')
            ),
        
        wellPanel(
            id='covid_filter_panel',
            style='padding: 5px',
            
            HTML("<b>How the Pandemic Affects HIV:</b>"),
            sim.counter,
            tags$br(),
            parameter.selector.panel,
            
            make.popover('covid_filter_panel',
                         title='Specify Ranges for How the Pandemic Affects HIV Parameters',
                         content="Drag the sliders to limit the ranges of possible effects of the COVID-19 pandemic on each parameter. Only simulations with parameters in those ranges will be included in projections.",
                         placement='right')
        )
    )
}

##--------------------##
##-- ERROR CHECKING --##
##--------------------##

MIN.N.COVID.SIMS = 20
check.covid.prerun.errors <- function(settings)
{
    n.sims = get.num.filtered.covid.sims(settings$filter)
    
    if (n.sims >= MIN.N.COVID.SIMS)
        NULL
    else
    {
        paste0("Fewer than ", MIN.N.COVID.SIMS, 
               " pre-run simulations have parameters that fall into the specified ranges.",
               " Please choose wider parameter ranges.")
    }
}

##---------------------------------##
##-- SETTING GETTERS AND SETTERS --##
##---------------------------------##

get.covid.prerun.settings <- function(input, suffix='prerun')
{
    list(intervention.codes=get.covid.scenario.selection(input, suffix),
         filter=get.covid.filter(input, suffix))
}

set.covid.prerun.to.settings <- function(session, input, settings, suffix='prerun')
{
    set.covid.scenario.selection(session, suffix, scenario=settings$intervention.codes)
    set.covid.filter(session, suffix, filter=settings$filter)
}


get.covid.scenario.selection <- function(input, suffix)
{
    input[[paste0('covid_scenario_', suffix)]]
}

set.covid.scenario.selection <- function(session, suffix, scenario)
{
    updateRadioButtons(session=session,
                       inputId=paste0('covid_scenario_', suffix),
                       selected=scenario)
}

get.covid.filter <- function(input, suffix)
{
    rv = lapply(names(COVID.PARAMETERS), function(pp.name){
        input[[paste0('covid_', pp.name, '_', suffix)]] / 100
    })
    names(rv) = names(COVID.PARAMETERS)
    rv
}

set.covid.filter <- function(session, suffix, filter)
{
    for (pp.name in names(filter))
    {
        updateSliderInput(session=session,
                          inputId = paste0('covid_', pp.name, '_', suffix),
                          value=filter[[pp.name]] * 100)
    }
}


##---------------------##
##-- THE SIM COUNTER --##
##---------------------##

setup.covid.sim.counter <- function(input, output, suffix='prerun')
{
    n.sim = get.num.filtered.covid.sims(get.covid.filter(input, suffix))
    output[[paste0("covid_sim_counter_", suffix)]] <- renderText({ 
        paste0(format(n.sim, big.mark=','), 
               ' simulation',
               ifelse(n.sim==1, ' has', 's have'),
               ' parameter values in range')
    })
}


get.num.filtered.covid.sims <- function(parameter.filter)
{
    sum(get.filter.mask(parameters.or.simset=COVID.PARAMETER.VALUES,
                        parameter.filter=parameter.filter))
}