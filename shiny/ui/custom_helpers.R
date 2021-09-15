MAX.N.SUBPOPULATIONS = 5

##------------------------##
##-- CREATE UI ELEMENTS --##
##------------------------##

create.location.input <- function(location,
                                  web.version.data,
                                  suffix,
                                  inline=T,
                                  popover=T)
{
    choices = web.version.data$locations
    names(choices) = get.location.name(choices)
    
    if (inline)
    {
        selector = inline.select.input(inputId=paste0("location_", suffix), 
                            label="Location",
                            choices=choices,
                            selected=location)
    }
    else
    {
        selector = selectInput(
            inputId=paste0("location_", suffix), 
            label="Location",
            choices=choices,
            selected=location,
            multiple=FALSE,
            selectize=TRUE, 
            width=NULL, 
            size=NULL)
    }
    
    if (popover)
    {
        holder.id = paste0('location_', suffix, '_holder')
        tags$div(
            id = holder.id,
            selector,
            
            make.popover(holder.id,
                         title=paste0('What ', web.version.data$location.resolute,
                                      ' to Project ', web.version.data$intervention.label, 's For'),
                         content=web.version.data$location.popover,
                         placement='right'),
            
        )
    }
    else
        selector
}

conditionalDropdown <- function(
    i,
    inputId.prefix,
    # box.title,
    selectInput.label,
    materialSwitch.label,
    choices,
    popover.title,
    popover.content,
    selected=NULL
) {
    panel.id = paste0(inputId.prefix, '_panel_', i)
    div(id = panel.id,
        fluidRow(
            
            checkboxInput(
                inputId=paste0(inputId.prefix, '_switch', i),
                label=materialSwitch.label, 
                value=FALSE)
            
            #            materialSwitch(
            #               inputId=paste0(inputId.prefix, '_switch', i),
            #              label=materialSwitch.label, 
            #             value=FALSE,
            #            right=TRUE,
            #           status='primary'
            #      ) # </materialSwitch>
        ),  #</fluidRow>
        fluidRow(conditionalPanel(
            condition=paste0(
                'input.', inputId.prefix, '_switch', i, ' == true'
            ),
            selectInput.label,
            selectInput(
                inputId=paste0(inputId.prefix, '_value', i),
                label=NULL,
                choices=choices,
                selectize = F,
                selected = selected
            )
            
        )),  # </conditionalPanel/fluidRow>
        make.popover(panel.id, title=popover.title, content=popover.content, placement='right')
    )  # /div>
}

create.custom.tpop.box <- function(i) {
    
    # collapse = TRUE
    # customIntervention_box_switch.value = FALSE
    # if (i == 1) {
    #  # collapse = FALSE
    #  customIntervention_box_switch.value = TRUE
    # }
    
    dimension.value.options = get.dimension.value.options(
        version=version,
        location=input$geographic_location,
        msm_idu_mode=TRUE)
    panel.id = paste0('custom_tpop_box_',i)
    fluidRow(id=panel.id,
             tags$div(
                 lapply(dimension.value.options,
                        function(dim) {
                            switchId = paste0('custom_tpop_all_', dim$code, '_', i)
                            materialSwitch.value = F
                            
                            fluidRow(
                                fluidRow(
                                    HTML(paste0('<b>', dim$label, '</b>'))
                                ),  # </fluidRow>
                                fluidRow(
                                    checkboxInput(
                                        inputId=switchId,
                                        label=paste0('Include All ', dim$label.plural),#tags$div(style='font-style: italic', paste0('Include All ', dim$label.plural)),
                                        value=materialSwitch.value),
                                    #      materialSwitch(
                                    #          inputId=switchId,
                                    #          label=paste0('Include all ', dim$label.plural),
                                    #          value=materialSwitch.value,
                                    #          right=TRUE,
                                    #          status='primary'),
                                    # Got 'undefined' error for input val, prolly cuz hadnt loaded
                                    conditionalPanel(
                                        condition=paste0('!input.', switchId),
                                        checkboxGroupInput(
                                            inputId=paste0('custom_tpop_', dim$code, '_', i),
                                            label=NULL,
                                            choiceNames=dim$names,
                                            choiceValues=dim$values,
                                            selected = NULL) 
                                    )
                                ),  # </fluidRow>
                                verticalSpacer(10)
                                
                            )})  # </fluidRow/lapply>
                 # ))  # </conditionalPanel/Box>
             ),  # </wellPanel>
             
             
             make.popover(panel.id,
                          title="Specify Subgroups",
                          content="Click on the number for each subgroup and select the age(s), race(s), sex(es), and risk factor(s) to target. Once you are done, specify the intervention in the panel to the right.",
                          placement='right')
    )  # </fluidRow>
}


create.custom.intervention.unit.selector <- function(i, web.version.data)
{
    year.options = get.year.options(web.version.data$name)
    tags$div(
        tags$div(
            
            # The date range
            tags$div(id=paste0('custom_date_range_', i),
                     selectInput(inputId = paste0('custom_int_from_', i),
                                 label = "Intervention Roll-Out Begins in:",
                                 choices = year.options$values,
                                 selected = year.options$values[4],
                                 selectize = F),
                     selectInput(inputId = paste0('custom_int_to_', i),
                                 label = "... and Is Fully Implemented by:",
                                 choices = year.options$values,
                                 selected = min(max(year.options$values), year.options$values[8]),
                                 selectize = F),
            ),
            
            make.popover(paste0('custom_date_range_', i),
                         title="Intervention Ramp-Up Time Frame",
                         content="Specify a year when the intervention starts rolling out (on Jan 1 of the year), and the year by which it is fully implemented (on Dec 31). Parameters will scale linearly over this time frame.",
                         placement='right'),
            
            # Testing
            conditionalDropdown(
                i=i,
                inputId.prefix='custom_testing',
                materialSwitch.label=tags$b("Intervene on Testing"),
                selectInput.label='Average number of tests per person:',
                choices=make.named.choices(choiceValues=TESTING.OPTIONS$values, 
                                           choiceNames=TESTING.OPTIONS$names),
                selected=TESTING.OPTIONS$default,
                popover.title="Freqency of HIV Testing",
                popover.content='Specify how frequently, on average, individuals in the targeted subgroup are tested.'
            ),
            
            # PrEP
            conditionalDropdown(
                i=i,
                inputId.prefix='custom_prep',
                materialSwitch.label=tags$b("Intervene on PrEP"),
                selectInput.label='PrEP Coverage:',
                choices=make.named.choices(choiceValues=PREP.OPTIONS$values, 
                                           choiceNames=PREP.OPTIONS$names),
                popover.title="PrEP Coverage",
                popover.content='Specify the proportion of individuals at risk for HIV acquisition in the subgroup who are enrolled in a PrEP program. This entails both a prescription for emtricitabine/tenofovir and testing every 3 months.'
            ),
            
            # Suppression
            conditionalDropdown(
                i=i,
                inputId.prefix='custom_suppression',
                materialSwitch.label=tags$b("Intervene on Suppression"),
                selectInput.label='Proportion of diagnosed PWH who are Suppressed:',
                choices=make.named.choices(choiceValues=SUPPRESSION.OPTIONS$values, 
                                           choiceNames=SUPPRESSION.OPTIONS$names),
                popover.title="Viral Suppression",
                popover.content='Specify the proportion of PWH (who are aware of their diagnosis) who are virally suppressed.'
            ),
            
            # Needle Exchange
            conditionalDropdown(
                i=i,
                inputId.prefix='custom_needleexchange',
                materialSwitch.label=tags$b("Intervene on Needle Exchange"),
                selectInput.label='Proportion of PWID in Needle Exchange Programs:',
                choices=make.named.choices(choiceValues=NEEDLE.EXCHANGE.OPTIONS$values, 
                                           choiceNames=NEEDLE.EXCHANGE.OPTIONS$names),
                popover.title="Needle Exchange",
                popover.content='Specify the proportion of actively using people who inject drugs (PWID) who are participating in a needle exchange program.'
            ),
            
            # MOUD
            conditionalDropdown(
                i=i,
                inputId.prefix='custom_moud',
                materialSwitch.label=tags$b("Intervene on MOUDs"),
                selectInput.label='Proportion of PWID on MOUDs:',
                choices=make.named.choices(choiceValues=MOUD.OPTIONS$values, 
                                           choiceNames=MOUD.OPTIONS$names),
                popover.title="Medications for Opioid Use Disorder (MOUDs)",
                popover.content='Specify the proportion of opioid-using people who inject drugs (PWID) who are taking a medication for opioid use disorder (MOUD) to maintain remission'
            )
            
            
        ) #</wellPanel
    )  # </div>
}

##------------------------##
##-- ADD EVENT HANDLERS --##
##------------------------##

add.custom.event.handlers <- function(session, input, output)
{
    # Custom interventions: Left panel tabPanel
    observeEvent(input$n_subpops, {
        for (i in 1:MAX.N.SUBPOPULATIONS)
        {
            if (i <= get.custom.n.subpopulations(input))
            {
                showTab(inputId="custom_tabset_panel", target=as.character(i))
                # showTab(inputId="custom_unit_tabset_panel", target=as.character(i))
            }
            else
            {
                hideTab(inputId="custom_tabset_panel", target=as.character(i))
                #  hideTab(inputId="custom_unit_tabset_panel", target=as.character(i))
            }
        }
    })
    
    lapply(1:MAX.N.SUBPOPULATIONS, function(i){
        
        add.year.range.dropdown.handler(session, input,
                                        from.id=paste0('custom_int_from_', i),
                                        to.id=paste0('custom_int_to_', i),
                                        min.delta=0)
        
    })
}

##----------------------##
##-- SETTINGS OBJECTS --##
##----------------------##

get.custom.settings <- function(input, web.version.data)
{
    rv = list(n.subpopulations = get.custom.n.subpopulations(input))
    
    rv$sub.populations =  lapply(1:rv$n.subpopulations, function(i){
        lapply(DIMENSION.VALUES.2, function(dim){
            do.get.custom.tpop.selection(input, num=i, dim=dim)
        })
    })
        
    rv$sub.units = web.version.data$get.custom.unit.settings.function(input, rv$n.subpopulations)
        
    rv
}

custom.settings.to.trackable <- function(settings, web.version.data)
{
    rv = list()
    
    
    rv = c(rv,
           web.version.data$custom.units.to.trackable.function(settings))
    
    rv
}

get.ehe.custom.unit.settings <- function(input, n.subpopulations)
{
    lapply(1:n.subpopulations, function(i){
        sub = list()
        
        sub$start.year = get.custom.start.year(input, num=i)
        sub$end.year = get.custom.end.year(input, num=i)
        
        sub$use.testing = get.custom.use.testing(input, num=i)
        sub$testing.frequency = get.custom.testing.frequency(input, num=i)
        
        sub$use.prep = get.custom.use.prep(input, num=i)
        sub$prep.uptake = get.custom.prep.uptake(input, num=i)
        
        sub$use.suppression = get.custom.use.suppression(input, num=i)
        sub$suppressed.proportion = get.custom.suppressed.proportion(input, num=i)
        
        sub$use.needle.exchange = get.custom.use.needle.exchange(input, num=i)
        sub$needle.exchange.proportion = get.custom.needle.exchange.proportion(input, num=i)
        
        sub$use.moud = get.custom.use.moud(input, num=i)
        sub$moud.proportion = get.custom.moud.proportion(input, num=i)
        
        sub
    })
}

set.custom.to.settings <- function(session, input, settings, web.version.data)
{
    # Set n subpop
    set.custom.n.subpopulations(session, settings$n.subpopulations)
    
    # Set subpop characteristics
    lapply(1:settings$n.subpopulations, function(i){
        
        sub = settings$sub.populations[[i]]
        
        lapply(DIMENSION.VALUES.2, function(dim){
            do.set.custom.tpop.selection(session, num=i, dim=dim, values=sub[[dim$code]])
        })
    })
    
    # Set subpop units
    web.version.data$set.custom.unit.settings.function(session, input, settings)
}

set.ehe.custom.units.to.settings <- function(session, input, settings)
{
    for (i in 1:settings$n.subpopulations)
    {
        sub = settings$sub.units[[i]]
        
        set.custom.year.range(session, input, 
                              num=i,
                              start.year = sub$start.year,
                              end.year = sub$end.year)
        
        set.custom.use.testing(session, num=i, use=sub$use.testing)
        set.custom.testing.frequency(session, num=i, testing.frequency = sub$testing.frequency)
        
        set.custom.use.prep(session, num=i, use = sub$use.prep)
        set.custom.prep.uptake(session, num=i, prep.uptake = sub$prep.uptake)
        
        set.custom.use.suppression(session, num=i, use=sub$use.needle.exchange)
        set.custom.suppressed.proportion(session, num=i, suppressed.proportion = sub$suppressed.proportion)
        
        set.custom.use.needle.exchange(session, num=i, use=sub$use.suppression)
        set.custom.needle.exchange.proportion(session, num=i, needle.exchange.proportion = sub$needle.exchange.proportion)
        
        set.custom.use.moud(session, num=i, use=sub$use.moud)
        set.custom.moud.proportion(session, num=i, moud.proportion = sub$moud.proportion)
    }
}

##-------------------------##
##-- GETTERS and SETTERS --##
##-------------------------##

#-- N SUBGROUPS --#
get.custom.n.subpopulations <- function(input)
{
    as.integer(input$n_subpops)
}

set.custom.n.subpopulations <- function(session, n.subpops)
{
    updateSelectInput(session,
                      inputId = 'n_subpops',
                      selected = n.subpops)
}


#-- TARGET POPULATION SPECIFICATION --#
get.custom.ages <- function(input, num)
{
    do.get.custom.tpop.selection(input, num, AGE.OPTIONS)
}

set.custom.ages <- function(session, num, ages)
{
    do.set.custom.tpop.selection(session,
                                 num=num,
                                 dim=AGE.OPTIONS,
                                 values=ages)
}


get.custom.races <- function(input, num)
{
    do.get.custom.tpop.selection(input, num, RACE.OPTIONS)
}

set.custom.races <- function(session, num, races)
{
    do.set.custom.tpop.selection(session,
                                 num=num,
                                 dim=RACE.OPTIONS,
                                 values=races)
}

get.custom.sexes <- function(input, num)
{
    do.get.custom.tpop.selection(input, num, SEX.OPTIONS)
}

set.custom.sexes <- function(session, num, sexes)
{
    do.set.custom.tpop.selection(session,
                                 num=num,
                                 dim=SEX.OPTIONS,
                                 values=sexes)
}


get.custom.risks <- function(input, num)
{
    do.get.custom.tpop.selection(input, num, RISK.OPTIONS.2)
}

set.custom.risks <- function(session, num, risks)
{
    do.set.custom.tpop.selection(session,
                                 num=num,
                                 dim=RISK.OPTIONS,
                                 values=risks)
}


do.get.custom.tpop.selection <- function(input, num,
                                         dim)
{
    all.id = paste0('custom_tpop_all_', dim$code, '_', num)
    if (input[[all.id]])
        dim$values
    else
    {
        selector.id = paste0('custom_tpop_', dim$code, '_', num)
        input[[selector.id]]
    }
}

do.set.custom.tpop.selection <- function(session, num,
                                         dim, values)
{
    all.id = paste0('custom_tpop_all_', dim$code, '_', num)
    all.selected = setequal(values, dim$values)
    
    updateCheckboxInput(session,
                        inputId = all.id,
                        value = all.selected)
    if (!all.selected)
    {
        selector.id = paste0('custom_tpop_', dim$code, '_', num)
        updateCheckboxGroupInput(session,
                                 inputId = selector.id,
                                 selected = values)
    }
}

##----------------------------##
##-- INTERVENTION INTENSITY --##
##----------------------------##

#-- Year Range --#
get.custom.start.year <- function(input, num)
{
    as.numeric(input[[paste0('custom_int_from_', num)]])
}

get.custom.end.year <- function(input, num)
{
    as.numeric(input[[paste0('custom_int_to_', num)]])
}

set.custom.year.range <- function(session, input, 
                                  num,
                                  start.year,
                                  end.year)
{
    update.year.range.dropdown(session, input,
                               id1 = paste0('custom_int_from_', num),
                               value1 = start.year,
                               id2 =  paste0('custom_int_to_', num),
                               value2 = end.year)
}

#-- Testing --#
get.custom.use.testing <- function(input, num)
{
    input[[paste0('custom_testing_switch', num)]]
}

set.custom.use.testing <- function(session, num, use)
{
    updateCheckboxInput(session,
                        inputId = paste0('custom_testing_switch', num),
                        value = use)
}


get.custom.testing.frequency <- function(input, num)
{
    as.numeric(input[[paste0('custom_testing_value', num)]])
}

set.custom.testing.frequency <- function(session, num, testing.frequency)
{
    updateSelectInput(session,
                      inputId = paste0('custom_testing_value', num),
                      selected = testing.frequency)
}

#-- PrEP --#
get.custom.use.prep <- function(input, num)
{
    input[[paste0('custom_prep_switch', num)]]
}

set.custom.use.prep <- function(session, num, use)
{
    updateCheckboxInput(session,
                        inputId = paste0('custom_prep_switch', num),
                        value = use)
}


get.custom.prep.uptake <- function(input, num)
{
    as.numeric(input[[paste0('custom_prep_value', num)]])
}

set.custom.prep.uptake <- function(session, num, prep.uptake)
{
    updateSelectInput(session,
                      inputId = paste0('custom_prep_value', num),
                      selected = prep.uptake)
}


#-- Suppression --#
get.custom.use.suppression <- function(input, num)
{
    input[[paste0('custom_suppression_switch', num)]]
}

set.custom.use.suppression <- function(session, num, use)
{
    updateCheckboxInput(session,
                        inputId = paste0('custom_suppression_switch', num),
                        value = use)
}


get.custom.suppressed.proportion <- function(input, num)
{
    as.numeric(input[[paste0('custom_suppression_value', num)]])
}

set.custom.suppressed.proportion <- function(session, num, suppressed.proportion)
{
    updateSelectInput(session,
                      inputId = paste0('custom_suppression_value', num),
                      selected = suppressed.proportion)
}

#-- Needle-Exchange --#
get.custom.use.needle.exchange <- function(input, num)
{
    input[[paste0('custom_needleexchange_switch', num)]]
}

set.custom.use.needle.exchange <- function(session, num, use)
{
    if (is.null(use))
        use = F
    
    updateCheckboxInput(session,
                        inputId = paste0('custom_needleexchange_switch', num),
                        value = use)
}


get.custom.needle.exchange.proportion <- function(input, num)
{
    as.numeric(input[[paste0('custom_needleexchange_value', num)]])
}

set.custom.needle.exchange.proportion <- function(session, num, needle.exchange.proportion)
{
    if (!is.null(needle.exchange.proportion))
        updateSelectInput(session,
                          inputId = paste0('custom_needleexchange_value', num),
                          selected = needle.exchange.proportion)
}


#-- MOUDs --#
get.custom.use.moud <- function(input, num)
{
    input[[paste0('custom_moud_switch', num)]]
}

set.custom.use.moud <- function(session, num, use)
{
    if (is.null(use))
        use = F
    
    updateCheckboxInput(session,
                        inputId = paste0('custom_moud_switch', num),
                        value = use)
}


get.custom.moud.proportion <- function(input, num)
{
    as.numeric(input[[paste0('custom_moud_value', num)]])
}

set.custom.moud.proportion <- function(session, num, moud.proportion)
{
    if (!is.null(moud.proportion))
        updateSelectInput(session,
                          inputId = paste0('custom_moud_value', num),
                          selected = moud.proportion)
}