

##----------------------##
##-- CREATE THE PANEL --##
##----------------------##

create.intervention.selector.panel <- function(suffix,
                                               lump.idu=T)
{
    # Make the Aspect Selector
    aspect.selector = make.intervention.aspect.selector(INTERVENTION.LIST,
                                                        suffix=suffix,
                                                        include.none=T)
    
    # Make the Target Population Selector
    tpop.selector = make.intervention.tpop.selector(INTERVENTION.LIST,
                                                    suffix=suffix)
    
    # Make the Time Frame Selector
    time.frame.selector = make.intervention.time.frame.selector(INTERVENTION.LIST,
                                                                suffix=suffix)
    
    # Make the Final Selector
    final.selector = make.intervention.final.selector(INTERVENTION.LIST,
                                                      suffix=suffix)
    
    
    # Put it all together and return
    tags$div(
        fluidRow(aspect.selector),
        fluidRow(tpop.selector),
        fluidRow(time.frame.selector),
        fluidRow(final.selector)
    )
}

##---------------------------------------------##
##-- GETTING/SETTING INPUT FROM/TO THE PANEL --##
##---------------------------------------------##

get.aspect.selector.id <- function(suffix)
{
    paste0("int_aspect_", suffix)
}

get.tpop.selector.id <- function(aspect.selection,
                                 suffix)
{
    paste0("int_tpop_", aspect.selection, "_", suffix)
}

get.time.frame.selector.id <- function(aspect.selection,
                                       tpop.selection,
                                       suffix)
{
    paste0('int_timeframe_', tpop.selection, "_", aspect.selection, "_", suffix) 
}

get.final.selector.id <- function(aspect.selection,
                                  tpop.selection,
                                  time.frame.selection,
                                  suffix)
{
    paste0('int_final_', time.frame.selection, "_", tpop.selection, "_", aspect.selection, "_", suffix)
}

get.intervention.selection <- function(input, suffix)
{
    aspect.selection = input[[get.aspect.selector.id(suffix)]]
    if (is.null(aspect.selection) || aspect.selection=='none')
        NULL
    else
    {
        tpop.selector.id = get.tpop.selector.id(aspect.selection=aspect.selection, suffix=suffix)
        tpop.selection = input[[tpop.selector.id]]
        
        
        time.frame.selector.id = get.time.frame.selector.id(aspect.selection=aspect.selection,
                                                            tpop.selection=tpop.selection,
                                                            suffix=suffix)
        time.frame.selection = input[[time.frame.selector.id]]
        
        
        final.selector.id = get.final.selector.id(aspect.selection=aspect.selection,
                                                  tpop.selection=tpop.selection,
                                                  time.frame.selection = time.frame.selection,
                                                  suffix=suffix)
        input[[final.selector.id]]
    }
}

set.intervention.selection <- function(session, suffix, int.code)
{
    mask = INTERVENTION.LIST$intervention.code == int.code

    if (is.null(int.code) || !any(mask))
        updateRadioButtons(session, 
                           inputId = get.aspect.selector.id(suffix), 
                           selected = 'none')
    else
    {
        index = (1:length(mask))[mask][1]
        
        # Aspect
        aspect.selection = INTERVENTION.LIST$unit.type.code[index]
        updateRadioButtons(session,
                           inputId = get.aspect.selector.id(suffix),
                           selected = aspect.selection)
        
        # Target Population
        tpop.selection = INTERVENTION.LIST$target.population.index[index]
        updateRadioButtons(session,
                           inputId = get.tpop.selector.id(aspect.selection=aspect.selection, suffix),
                           selected = tpop.selection)
        
        # Time Frame
        time.frame.selection = INTERVENTION.LIST$time.frame.code[index]
        updateRadioButtons(session,
                           inputId = get.time.frame.selector.id(aspect.selection=aspect.selection,
                                                                tpop.selection=tpop.selection,
                                                                suffix=suffix),
                           selected = time.frame.selection)
        
        # Final
        final.selector.id = get.final.selector.id(aspect.selection=aspect.selection,
                                                  tpop.selection=tpop.selection,
                                                  time.frame.selection=time.frame.selection,
                                                  suffix=suffix)
        updateRadioButtons(session,
                           inputId = final.selector.id,
                           selected = int.code)
    }
}

##----------------------------------##
##-- HELPERS FOR MAKING THE PANEL --##
##----------------------------------##

make.intervention.aspect.selector <- function(int.list,
                                              suffix,
                                              include.none=T)
{
    unique.unit.types = unique(int.list$unit.type)
    
    unit.choice.values = lapply(unique.unit.types, unit.type.category.code)
    
    unit.choice.names = sapply(unique.unit.types, unit.types.to.pretty.name)
    
    unit.choice.values = c(list('none'), unit.choice.values)
    unit.choice.names = c(list('None'), unit.choice.names)
    
    names(unit.choice.values) = names(unit.choice.names) = NULL
    
    
    id = get.aspect.selector.id(suffix)
    tags$div(
        radioButtons(inputId = id,
                     label='Which Aspects to Intervene On:',
                     choiceNames=unit.choice.names,
                     choiceValues=unit.choice.values,
                     selected='none'
        ),
        
        make.popover(id,
                     title='What Should the Intervention Affect?',
                     content="You can choose interventions that affect HIV testing, PrEP among those at risk for HIV acquisition, viral suppression among PWH, participation in needle-exchange programs, MOUDs (medications for opioid use disorder), or combinations of these",
                     placement='right')
    )
}

make.intervention.tpop.selector <- function(int.list,
                                            suffix)
{
    unique.unit.type.codes = unique(int.list$unit.type.code)
    
    selectors = lapply(unique.unit.type.codes, function(unit.type.code){
            
            mask = int.list$unit.type.code == unit.type.code
            
            unique.tpop.codes.for.unit.type = unique(int.list$target.population.code[mask])
            
            choice.values = (1:length(int.list$unique.target.population.codes))[
                sapply(int.list$unique.target.population.codes, function(codes){
                    any(sapply(unique.tpop.codes.for.unit.type, function(codes2){
                        length(codes)==length(codes2) && all(codes==codes2)
                    }))
                })]
            choice.names = lapply(unique.tpop.codes.for.unit.type, function(codes){
                target.population.codes.to.pretty.name(codes)
            })
            names(choice.names) = names(choice.values) = NULL
            
            
            id = get.tpop.selector.id(aspect.selection=unit.type.code,
                                      suffix=suffix)
            
            radios = radioButtons(inputId = id,
                                  label='Target Subgroup(s):',
                                  choiceNames=choice.names,
                                  choiceValues=choice.values,
                                  selected=choice.values[1])
            
            conditionalPanel(
                condition = paste0("input.", get.aspect.selector.id(suffix)," == '", unit.type.code, "'"),
                
                radios,
                
                make.popover(id,
                             title='What Subgroups Should the Intervention Target?',
                             content="Choose which population subgroups the intervention should be deployed in.",
                             placement='right')
            )
    })
    
    do.call(tags$div, selectors)
}

make.intervention.time.frame.selector <- function(int.list,
                                                  suffix)
{
    unique.unit.type.codes = unique(int.list$unit.type.code)
    
    selectors = lapply(1:length(int.list$unique.target.population.codes), function(tpop.index){
        rv = lapply(unique.unit.type.codes, function(unit.type.code){
            
            mask = int.list$target.population.index == tpop.index &
                int.list$unit.type.code == unit.type.code
            
            if (any(mask))
            {
                choice.values = unique(int.list$time.frame.code[mask])
                time.frames = lapply(choice.values, function(code){
                    int.list$time.frame[int.list$time.frame.code==code][[1]]
                })
                choice.names = lapply(time.frames, function(time.frame){
                    paste0(time.frame[1], " to ", (time.frame[2]-1))
                })
                names(choice.names) = names(choice.values) = NULL
                
                id = get.time.frame.selector.id(aspect.selection=unit.type.code,
                                                tpop.selection = tpop.index,
                                                suffix=suffix)
                radios = radioButtons(inputId=id,
                                      label='Intervention Roll-Out:',
                                      choiceNames=choice.names,
                                      choiceValues=choice.values,
                                      selected=choice.values[1])
                
                conditionalPanel(
                  #  condition = paste0("input.", get.tpop.selector.id(unit.type.code, suffix=suffix), " == ", tpop.index),
                #    condition = paste0("input.", get.aspect.selector.id(suffix)," == '", unit.type.code, "'"),
                                       
                    condition = paste0("input.", get.aspect.selector.id(suffix)," == '", unit.type.code,
                                       "' && input.", get.tpop.selector.id(unit.type.code, suffix=suffix), " == ", tpop.index),
                    radios,
                    
                    make.popover(id,
                                 title='Over What Time Frame Should Interventions Should be Rolled out?',
                                 content="Choose the time frame over which the intervention is ramped up (the year when it starts, on Jan 1, to the year when it is fully implemented, as of Dec 31)",
                                 placement='right')
                )
            }
            else
                NULL
        })
        
        rv = rv[!sapply(rv, is.null)]
        rv
    })
    
    do.call(tags$div, selectors)
}


make.intervention.final.selector <- function(int.list,
                                             suffix)
{
    unique.unit.type.codes = unique(int.list$unit.type.code)
    unique.time.frame.codes = unique(int.list$time.frame.code)
    
    selectors = lapply(1:length(int.list$unique.target.population.codes), function(tpop.index){
        lapply(unique.unit.type.codes, function(unit.type.code){
            aspect.tpop.mask = int.list$target.population.index == tpop.index &
                int.list$unit.type.code == unit.type.code
            rv = lapply(unique(int.list$time.frame.code[aspect.tpop.mask]), function(time.frame.code){
                        
                    mask = aspect.tpop.mask & int.list$time.frame.code == time.frame.code
                    
                    if (any(mask))
                    {
                        choice.values = int.list$intervention.code[mask]
                        choice.names = lapply(int.list$intervention.lumped.idu[mask], intervention.brief.description)
                        choice.names = lapply(choice.names, function(name){tags$div(lump.idu.in.name(name))})
                        names(choice.names) = names(choice.values) = NULL
                        
                        id = get.final.selector.id(aspect.selection = unit.type.code,
                                                   tpop.selection = tpop.index,
                                                   time.frame.selection = time.frame.code,
                                                   suffix=suffix)
                        radios = radioButtons(inputId=id,
                                     label='Intensity of Interventions:',
                                     choiceNames=choice.names,
                                     choiceValues=choice.values,
                                     selected=choice.values[1])
                        
                        time.frame.id = get.time.frame.selector.id(aspect.selection = unit.type.code,
                                                                   tpop.selection = tpop.index,
                                                                   suffix=suffix)
                        conditionalPanel(
                            condition = paste0("input.", get.aspect.selector.id(suffix)," == '", unit.type.code, 
                                               "' && input.", get.tpop.selector.id(unit.type.code, suffix=suffix), " == ", tpop.index,
                                               " && input.", time.frame.id, " == '", time.frame.code, "'"),
                            radios,
                            
                            make.popover(id,
                                         title='What Intensity of Interventions Should be Applied?',
                                         content="Choose the specific levels of different interventions (HIV testing, PrEP coverage among those at risk for HIV acquisition, viral suppression among PWH, etc. to apply to the targeted subgroups.",
                                         placement='right')
                        )
                    }
                    else
                        NULL
            })            
            
            rv = rv[!sapply(rv, is.null)]
            rv
        })
    })
    
    do.call(tags$div, selectors)
}


##-------------##
##-- HELPERS --##
##-------------##

##-- READING THE INTERVENTION LIST --##

#returns a list with two elements
# $location - a vector of location ids
# $intervention - a list of interventions
get.interventions.list <- function(include.no.intervention=F,
                                   disregard.location=T,
                                   lump.idu=T)
{
    rv = get.prerun.intervention.codes()   

    if (disregard.location)
        rv = list(intervention.code = unique(rv$intervention.code))

    rv$intervention = lapply(rv$intervention.code, intervention.from.code)
    
    if (!include.no.intervention)
    {
        mask = !sapply(rv$intervention, is.null.intervention)
        rv$intervention = rv$intervention[mask]
        rv$intervention.code = rv$intervention.code[mask]
        rv$location = rv$location[mask]
    }
    
    o = order.interventions(rv$intervention)
    rv$intervention = rv$intervention[o]
    rv$intervention.code = rv$intervention.code[o]
    rv$location = rv$location[o]
    
    # use the categories, since they may be named differently from the 'type' per se
    rv$unit.type = lapply(rv$intervention, function(int){
        sort(unique(unlist(sapply(int$raw, function(sub){
            sapply(sub$intervention.units, function(u){
                u$name.metadata$category
            })
        }))))
    })
    
    rv$unit.type.code = sapply(rv$unit.type, unit.type.category.code)
 
    rv$unit.type.names = unique(unlist(rv$unit.type))
    names(rv$unit.type.names) = sapply(rv$unit.type.names, unit.type.category.code)
    
    if (lump.idu)
        rv$intervention.lumped.idu = lapply(rv$intervention, lump.idu.for.intervention)
    else
        rv$intervention.lumped.idu = rv$interventions
    
    rv$target.population.code = lapply(
        rv$intervention.lumped.idu, function(int) {
            sapply(
                get.target.populations.for.intervention(int), 
                target.population.to.code)
        })
    
    rv$unique.target.population.codes = unique(rv$target.population.code)
    rv$target.population.index = sapply(rv$target.population.code, function(tpop1){
        mask = sapply(rv$unique.target.population.codes, function(tpop2){
            setequal(tpop1, tpop2)
        })
        (1:length(rv$unique.target.population.codes))[mask]
    })
    
    rv$time.frame = lapply(rv$intervention, function(int){
        if (is.null.intervention(int))
            c(NA,NA)
        else
        {
            all.start.times = unlist(sapply(int$raw, function(sub){
                unlist(sapply(sub$intervention.units, function(u){
                    u$start.year
                }))
            }))
            
            all.times = unlist(sapply(int$raw, function(sub){
                unlist(sapply(sub$intervention.units, function(u){
                    u$years
                }))
            }))
            
            c(min(all.start.times), max(all.times))
        }
    })
    
    rv$time.frame.code = sapply(rv$time.frame, function(time.frame){
        paste0(time.frame, collapse='')
    })
    
    rv
}

##-- UNIT TYPE --##

unit.types.to.pretty.name <- function(unit.types)
{
#    unit.types = int.list$unit.type.names[unit.types]
    if (length(unit.types)==1)
        paste0(unit.types, " only")
    else if (length(unit.types)==2)
        paste0(unit.types[1], " and ", unit.types[2])
    else
        paste0(paste0(unit.types[-length(unit.types)], collapse=", "),
               ", and ", unit.types[length(unit.types)])
}

unit.type.category.code <- function(unit.type.categories)
{
    codes = gsub("\\.", "", unit.type.categories)
    codes = gsub("_", "", codes)
    codes = gsub("\\s", "", codes)
    codes = tolower(codes)
    
    paste0(codes, collapse="_")
}


##-- INTERVENTION --##

intervention.brief.description <- function(int, include.start.text=F)
{
    HTML(get.intervention.description.by.target(int, 
                                                include.start.text=include.start.text,
                                                pre="<table>",
                                                post="</table>",
                                                bullet.pre = "<tr><td style='vertical-align: text-top; word-wrap:break-word;'>&#149;&nbsp;</td><td>",
                                                bullet.post = "</td></tr>",
                                                tpop.delimeter = '',
                                                unit.delimiter = ', ',
                                                pre.header = "<u>",
                                                post.header = ":</u> "
    ))
}

##-- TARGET POPULATION --##

target.population.codes.to.pretty.name <- function(tpop.codes)
{
    if (length(tpop.codes)==1 && tpop.codes=='none')
    {
        bullet = ''
        content = 'None'
    }
    else
    {
        bullet = paste0("<td style='vertical-align: text-top;'>&#149;&nbsp;&nbsp;</td>")
        tpops = lapply(tpop.codes, target.population.from.code)
        content = sapply(tpops, function(tpop){
            lump.idu.in.name(target.population.name(tpop))})
        
        content[-length(content)] = paste0(content[-length(content)], " <i>plus</i>")
    }
    #paste0("", paste0("&#149; ", sapply(tpops, target.population.name), collapse='<BR>'), "")
    HTML(paste0("<table>",
                paste0("<tr>",
                       bullet,
                       "<td style='text-align: left'>",
                       content,
                       "</td></tr>", collapse=''), 
                "</table>"))
}




##----------------------------------##
##-- SET UP THE INTERVENTION LIST --##
##----------------------------------##


# Make and save the intervention list to be universally available
#  Note - this is executed on launching the app
print("Making Intervention List")
INTERVENTION.LIST = get.interventions.list(disregard.location=T) 
#for now, we just assume that every location has every intervention
# and just assume that every location has every intervention