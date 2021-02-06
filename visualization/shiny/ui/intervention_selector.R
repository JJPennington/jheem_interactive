
create.intervention.selector.panel <- function(suffix,
                                               lump.idu=T)
{
    print('Creating Intervention Selector Panel')
    int.list = get.interventions.list(disregard.location=T) 
        #for now, we will not use this full list
        # and just assume that every location has every intervention
        

    # Make the Aspect Selector
    aspect.selector = make.intervention.aspect.selector(int.list,
                                                        suffix=suffix,
                                                        include.none=T)
    
    # Make the Target Population Selector
    tpop.selector = conditionalPanel(condition=paste0("input.int_aspect_", suffix," != 'none'"),
                                     make.intervention.tpop.selector(int.list,
                                                                     suffix=suffix))
    
    # Make the Final Selector
    final.selector = make.intervention.final.selector(int.list,
                                                      suffix=suffix)
    
    
    # Put it all together and return
    tags$div(
        fluidRow(aspect.selector),
        fluidRow(tpop.selector),
        fluidRow(final.selector)
    )
}

make.intervention.aspect.selector <- function(int.list,
                                              suffix,
                                              include.none=T)
{
    unique.unit.types = unique(int.list$unit.type)
    
    unit.choice.values = lapply(unique.unit.types, unit.type.code)
    
    unit.choice.names = lapply(unique.unit.types, unit.types.to.pretty.name)
    
    unit.choice.values = c(list('none'), unit.choice.values)
    unit.choice.names = c(list('None'), unit.choice.names)
    
    names(unit.choice.values) = names(unit.choice.names) = NULL
    
    
    radioButtons(inputId = paste0('int_aspect_', suffix),
                 label='Which Aspects to Intervene On:',
                 choiceNames=unit.choice.names,
                 choiceValues=unit.choice.values,
                 selected='none'
    )
}

make.intervention.tpop.selector <- function(int.list,
                                            suffix)
{
    tpop.choice.names = lapply(int.list$unique.target.population.codes, function(codes){
        target.population.codes.to.pretty.name(codes)
    })

    tpop.choice.values = 1:length(int.list$unique.target.population.codes)
    
    names(tpop.choice.names) = names(tpop.choice.values) = NULL
    
    
    radioButtons(inputId = paste0('int_tpop_', suffix),
                 label='Target Population(s):',
                 choiceNames=tpop.choice.names,
                 choiceValues=tpop.choice.values,
                 selected=tpop.choice.values[1]
    )
}

make.intervention.final.selector <- function(int.list,
                                             suffix)
{
    unique.unit.type.codes = unique(int.list$unit.type.code)
    
    selectors = lapply(1:length(int.list$unique.target.population.codes), function(tpop.index){
        lapply(unique.unit.type.codes, function(unit.type.code){
            
            mask = int.list$target.population.index == tpop.index &
                int.list$unit.type.code == unit.type.code
            
            choice.values = int.list$intervention.code[mask]
            choice.names = lapply(int.list$intervention.lumped.idu[mask], intervention.brief.description)
            choice.names = lapply(choice.names, function(name){tags$div(lump.idu.in.name(name))})
            names(choice.names) = names(choice.values) = NULL
            
            radios = radioButtons(inputId=paste0('int_', tpop.index, "_", unit.type.code),
                         label='Specific Interventions:',
                         choiceNames=choice.names,
                         choiceValues=choice.values,
                         selected=choice.values[1])
            
            conditionalPanel(
                condition = paste0("input.int_aspect_", suffix," == '", unit.type.code, "' && input.int_tpop_", suffix, " == ", tpop.index),
                radios 
            )
            
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
    
    rv$unit.type = lapply(rv$intervention, function(int){
        sort(get.intervention.unit.types(int))
    })
    rv$unit.type.code = sapply(rv$unit.type, unit.type.code)
 #   rv$unique.unit.type.codes = unique(rv$unit.type.code)
    
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
    
    
    rv
}

##-- UNIT TYPE --##

unit.types.to.pretty.name <- function(unit.types)
{
    unit.types = get.pretty.unit.type.names(unit.types)
    if (length(unit.types)==1)
        paste0(unit.types, " only")
    else if (length(unit.types)==2)
        paste0(unit.types[1], " and ", unit.types[2])
    else
        paste0(paste0(unit.types[-length(unit.types)], collapse=", "),
               ", and ", unit.types[length(unit.types)])
}

unit.type.code <- function(unit.types)
{
    paste0(unit.types, collapse="_")
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


##-- LUMPING IDU --##

lump.idu.for.intervention <- function(int)
{
    tpops = get.target.populations.for.intervention(int)
    mapped = lapply(tpops, function(tpop){
        lumped = tpop
        lumped[,,,'IDU_in_remission'] = lumped[,,,'active_IDU']
        if (target.populations.equal(tpop, lumped))
            tpop
        else
        {
            lumped.already.present = any(sapply(int$raw, function(sub){
                any(sapply(sub$target.populations, target.populations.equal, tpop))
            }))
            
            if (lumped.already.present)
                lumped
            else
                tpop
        }
    })
    names(mapped) = sapply(tpops, target.population.hash)
    
    for (i in 1:length(int$raw))
    {
        for (j in 1:length(int$raw[[i]]$target.populations))
            int$raw[[i]]$target.populations[[j]] = 
                mapped[[target.population.hash(int$raw[[i]]$target.populations[[j]]) ]]
    }
    
    int
}

lump.idu.in.name <- function(name)
{
    gsub('active and prior IDU', 'IDU', name, ignore.case = T)
}


