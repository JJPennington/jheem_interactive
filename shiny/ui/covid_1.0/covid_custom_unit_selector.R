
create.covid.custom.intervention.unit.selector <- function(i, web.version.data)
{
    start.id = paste0('covid_custom_start_',i)
    start.holder.id = paste0('covid_custom_start_holder_',i)
    start.input = tags$div(
        id = start.holder.id,
        dateInput(
            inputId = start.id,
            label='The Pandemic Starts to Affect HIV on:',
            value='2020-03-01',
            min='2020-02-01',
            max='2020-06-30'),
        
        make.popover(start.holder.id,
                     title=paste0("Specify When the Pandemic&#39;s Effects on HIV Start"),
                     content="Select the date on which the COVID-19 Pandemic starts having its effect on the parameters chosen below ",
                     placement='right')
    )
    
    parameter.inputs = lapply(COVID.PARAMETERS, function(pp){
        
        #The check box
        check.id = paste0('custom_', pp$id, '_', i)
        pp.check = checkboxInput(
            inputId = check.id,
            label = HTML(paste0('<b>', pp$parameter.label, '</b>'))
        )
        
        check.wrapper.id = paste0('custom_', pp$id, '_wrapper_', i)
        pp.check.wrapper = tags$div(
            id=check.wrapper.id,
            pp.check
        )
        
        effect.id.for.popover = paste0('custom_', pp$id, '_effect_holder_', i)
        effect.picker.id = paste0('custom_', pp$id, '_effect_',i)
        effect.picker = tags$div(
            id = effect.id.for.popover,
            paste0("The pandemic ", pp$effect.label, " ", tolower(pp$parameter.label), " by:"),
            sliderInput(
                inputId=effect.picker.id,
                label=NULL,#pp$label,
                min=100*pp$min,
                max=100*pp$max,
                value=100*c(pp$min, pp$max),
                post='%'
            ),
            
            make.popover(effect.id.for.popover,
                         title=paste0("Specify Pandemic&#39;s Effect on ", pp$parameter.label),
                         content=paste0("Drag the sliders to specify the range of possible effects on ",
                                        tolower(pp$parameter.label),
                                        ". Simulations will sample different effects within this range."),
                         placement='right')
        )
        
        start.normalize.id = paste0('custom_', pp$id, '_start_norm_', i)
        start.normalize.picker = tags$div(
            paste0("It begins to normalize on:"),
            dateInput(
                inputId = start.normalize.id,
                label=NULL,
                value='2021-03-04',
                min='2020-02-01',
                max='2024-12-31'
        ))
        
        normalize.span.choices = COVID.NORMALIZE.SPAN.OPTIONS$values
        names(normalize.span.choices) = COVID.NORMALIZE.SPAN.OPTIONS$names
        normalize.span.id = paste0('custom_', pp$id, '_norm_span_', i)
        normalize.span.picker = tags$div(
            paste0('... and is fully normal after:'),
            selectInput(
                inputId = normalize.span.id,
                label = NULL,
                choices = normalize.span.choices,
                selected = COVID.NORMALIZE.SPAN.OPTIONS$default
            )
        )
        
        time.pickers.id = effect.id.for.popover = paste0('custom_', pp$id, '_time_holder_', i)
        time.pickers = tags$div(
            id = time.pickers.id,
            
            start.normalize.picker,
            normalize.span.picker,
            
            make.popover(time.pickers.id,
                         title=paste0("Specify When and How Quickly ", pp$parameter.label, " Normalizes"),
                         content=paste0("Select the date on which ", 
                                        tolower(pp$parameter.label), 
                                        " begins to get back to normal and how long it takes to fully return to normal"),
                         placement='right')
        )
        
        tags$div(
            pp.check.wrapper,
            conditionalPanel(
                condition = paste0("input.", check.id),
                
                effect.picker,
                time.pickers
            ),
            
            make.popover(check.wrapper.id,
                         title=paste0('Specify How the Pandemic Affects ', pp$parameter.label),
                         content=paste0("Select whether the pandemic affects ", tolower(pp$parameter.label),
                                        ". If so, set the time-frame and the range of possible pandemic effects effects."),
                         placement='right')
        )
    })
    
    
    parameter.title.id = paste0('covid_param_panel_', i)
    tags$div(
        start.input,
        tags$br(),
        tags$div(
            id = parameter.title.id,
            HTML("<b>Allow the Pandemic to Affect:</b>")
        ),
        parameter.inputs,
            
        make.popover(parameter.title.id,
                     title='Specify How the Pandemic Affects HIV Parameters',
                     content="Select which HIV parameters the pandemic should affect. Set a time-frame and range of possible effects of the COVID-19 pandemic on each parameter. Simulations will sample different effects within these ranges.",
                     placement='right')
    )
}

##-------------------------##
##-- GETTERS and SETTERS --##
##-------------------------##

get.custom.covid.unit.settings <- function(input, n.subpopulations)
{
    lapply(1:n.subpopulations, function(i){
        
        sub = list()
        
        # start time
        start.id = paste0('covid_custom_start_',i)
        sub$start.time = input[[start.id]]
        
        # each parameter
        sub$parameters = lapply(COVID.PARAMETERS, get.custom.covid.unit.for.parameter, input=input, i=i)
        names(sub$parameters) = names(COVID.PARAMETERS)
        
        # return
        sub
    })
}

set.custom.covid.unit.settings <- function(session, input, settings)
{
    for (i in 1:settings$n.subpopulations)
    {
        sub = settings$sub.units[[i]]
        
        start.id = paste0('covid_custom_start_',i)
        updateDateInput(session, inputId = start.id, sub$start.time)
        
        for (pp in COVID.PARAMETERS)
            set.custom.covid.unit.for.parameter(session=session, 
                                                unit.param.settings = sub$parameters[[pp$name]],
                                                pp=pp, i=i)
    }
}


get.custom.covid.unit.for.parameter <- function(input, pp, i)
{
    rv = list()
    
    check.id = paste0('custom_', pp$id, '_', i)
    rv$use = input[[check.id]]
    
    effect.picker.id = paste0('custom_', pp$id, '_effect_',i)
    rv$effect = input[[effect.picker.id]] / 100
    
    start.normalize.id = paste0('custom_', pp$id, '_start_norm_', i)
    rv$start.normalize.time = input[[start.normalize.id]]
    
    normalize.span.id = paste0('custom_', pp$id, '_norm_span_', i)
    rv$normalize.span = input[[normalize.span.id]]
    
    rv
}


set.custom.covid.unit.for.parameter <- function(session, 
                                                unit.param.settings,
                                                pp, i)
{
    check.id = paste0('custom_', pp$id, '_', i)
    updateCheckboxInput(session,
                        inputId = check.id,
                        value = unit.param.settings$use)

    effect.picker.id = paste0('custom_', pp$id, '_effect_',i)
    updateSliderInput(session,
                      inputId = effect.picker.id,
                      value = unit.param.settings$effect * 100)

    start.normalize.id = paste0('custom_', pp$id, '_start_norm_', i)
    updateDateInput(session,
                    inputId = start.normalize.id,
                    value = unit.param.settings$start.normalize.time)
    
    normalize.span.id = paste0('custom_', pp$id, '_norm_span_', i)
    updateSelectInput(session,
                      inputId = normalize.span.id,
                      value = unit.param.settings$normalize.span)
    
}


check.covid.custom.unit.errors <- function(settings, i)
{
    sub = settings$sub.units[[i]]
    start.time = sub$start.time
    use.param = sapply(COVID.PARAMETERS, function(pp){
        sub$parameters[[pp$name]]$use
    })
    
    if (!any(use.param))
        "At least one parameter must be chosen to be affected by the pandemic"
    else
    {
        pp.to.use = COVID.PARAMETERS[use.param]
        invalid.time.errors = sapply(pp.to.use, function(pp){
            sub.param = sub$parameters[[pp$name]]
            if (sub.param$start.normalize.time < start.time)
                paste0("The time at which the ", pp$label, " begins to normalize has been set to",
                       " before the pandemic begins to affect it.")
            else
                NA
        })
        
        invalid.time.errors = invalid.time.errors[!is.na(invalid.time.errors)]
        if (length(invalid.time.errors)==0)
            NULL
        else
            invalid.time.errors
    }
}

 
get.covid.intervention.from.settings <- function(settings)
{
    rv = settings
    
    for (i in 1:rv$n.subpopulations)
    {
        rv$sub.populations[[i]] = get.custom.subpopulation(settings, i)
        
        rv$sub.units[[i]]$start.time = date.to.numeric.year(rv$sub.units[[i]]$start.time)
        
        for (j in 1:length(rv$sub.units[[i]]$parameters))
        {
            rv$sub.units[[i]]$parameters[[j]]$normalize.span = as.numeric(
                rv$sub.units[[i]]$parameters[[j]]$normalize.span)
            
            rv$sub.units[[i]]$parameters[[j]]$start.normalize.time = 
                date.to.numeric.year(rv$sub.units[[i]]$parameters[[j]]$start.normalize.time)
        }
    }
    
    class(rv) = 'covid.intervention'
    rv
}

##-------------------------------##
##-- FOR WEB VERSION INTERFACE --##
##-------------------------------##

covid.interventions.equal <- function(int1, int2)
{
    if (is(int1, 'covid.intervention') && is(int2, 'covid.intervention'))
    {
        if (int1$n.subpopulations != int2$n.subpopulations)
            F
        else
        {
            for (i in 1:int1$n.subpopulations)
            {
                if (int1$sub.populations[[i]] != int2$subpopulations[[i]])
                    return (F)
                
                sub.u1 = int1$sub.units[[i]]
                sub.u2 = int2$sub.units[[i]]
                
                if (sub.u1$start.time != sub.u2$start.time)
                    return (F)
                
                for (j in 1:length(sub.u1$parameters))
                {
                    sub.p1 = sub.u1$parameters[[j]]
                    sub.p2 = sub.u2$parameters[[j]]
                    
                    if (sub.p1$use != sub.p2$use ||
                        all(sub.p1$effect != sub.p2$effect) ||
                        sub.p1$start.normalize.time != sub.p2$start.normalize.time ||
                        sub.p1$normalize.span != sub.p2$normalize.span)
                        return (F)
                }
            }
            
            T
        }
    }
    else
        F
}

make.covid.scenario.summary = function()
{
    
}