

create.intervention.selector.panel <- function(suffix,
                                               lump.idu=T)
{
    print('Creating Intervention Selector Panel')
    interventions = get.interventions.list() 
        #for now, we will not use this full list
        # and just assume that every location has every intervention
        
    interventions = extract.unique.interventions.from.list(interventions)
    
    
    # Make the Aspect Selector
    aspect.selector = make.intervention.aspect.selector(interventions,
                                                        suffix=suffix)
    
    # Make the Target Population Selector
    tpop.selector = make.intervention.tpop.selector(interventions,
                                                    suffix=suffix)
    
    # Make the Final Selector
    final.selector = make.intervention.final.selector(interventions,
                                                      suffix=suffix)
    
    
    # Put it all together and return
    tags$div(
        fluidRow(aspect.selector),
        fluidRow(tpop.selector),
        fluidRow(final.selector)
    )
}

make.intervention.aspect.selector <- function(interventions,
                                              suffix,
                                              include.none=T)
{
    radioButtons(inputId = paste0('int_aspect_', suffix),
                      label='Which Aspects to Intervene On',
                      choices=c('one','two','three'))
}

make.intervention.tpop.selector <- function(interventions,
                                            suffix)
{
    radioButtons(inputId = paste0('int_aspect_', suffix),
                      label='Target Population(s):',
                      choices=c('one','two','three'))
}

make.intervention.final.selector <- function(interventions,
                                             suffix)
{
    radioButtons(inputId = paste0('int_aspect_', suffix),
                      label='Specific Interventions:',
                      choices=c('one','two','three'))
}

make.location.intervention.selector.panel <- function(location,
                                                      interventions,
                                                      suffix,
                                                      lump.idu)
{
    #-- Create a mapping for interventions to target populations and target populations to interventions --#
    
    interventions = interventions$intervention[interventions$location == location]

    if (lump.idu)
        interventions.lumped.idu = lapply(interventions, lump.idu.for.intervention)
    else
        interventions.lumped.idu = interventions
    
    target.population.codes.for.intervention = lapply(
        interventions.lumped.idu, function(int) {
            sapply(
                get.target.populations.for.intervention(int), 
                target.population.to.code)
        })
    
    unique.tpop.codes = unique(target.population.codes.for.intervention)
    
    interventions.for.unique.tpop.codes = lapply(
        unique.tpop.codes, function(tpop.codes){
            mask = sapply(target.population.codes.for.intervention, setequal, tpop.codes)
            interventions[mask]
        })
    
    lumped.idu.interventions.for.unique.tpop.codes = lapply(
        unique.tpop.codes, function(tpop.codes){
            mask = sapply(target.population.codes.for.intervention, setequal, tpop.codes)
            interventions.lumped.idu[mask]
        })
                          
    
    
    
    #-- Create the Components for the Input --#
    
    # The T-Pop Selector
    tpop.choice.names = lapply(c(list('none'),unique.tpop.codes), function(name){
        target.population.codes.to.pretty.name(name, font.size = TAB1.TITLE.FONT.SIZE)
    })
    tpop.choice.values = c(list('none'), lapply(1:length(unique.tpop.codes), function(i){
        paste0("int", num, "_tpop", i)
    }))
    names(tpop.choice.names) = names(tpop.choice.values) = NULL
    
    
    
    
    
    
    
    do.call(conditionalPanel, c(list(condition=paste0("input.", 'xxx'," == 'xxx")),
                                tpop.panels))
       
    conditionalPanel(
        style="height:100%",
        condition=paste0("input.location_",suffix,"=='", location,"'"),
        tpop.panels
    )
}

make.tpop.intervention.selector.panel <- function()
{
    
}

make.aspect.intervention.selector.panel <- function()
{
    
}


##-------------##
##-- HELPERS --##
##-------------##

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

unit.types.to.pretty.name <- function(unit.types, font.size='200%')
{
    unit.types = get.pretty.unit.type.names(unit.types)
    if (length(unit.types)==1)
        unit.types = paste0(unit.types, ' only')
    #paste0("", paste0("&#149; ", sapply(tpops, target.population.name), collapse='<BR>'), "")
    HTML(paste0("<table>", paste0("<tr><td style='vertical-align: text-top; font-size: ", font.size, 
                                  "'>&#149;&nbsp;&nbsp;</td>",
                                  "<td style='text-align: left; font-style: italic; font-size: ", font.size, "'>",
                                  unit.types, 
                                  "</td></tr>", collapse=''), 
                "</table>"))
}

target.population.codes.to.pretty.name <- function(tpop.codes, font.size='1em')
{
    if (length(tpop.codes)==1 && tpop.codes=='none')
    {
        bullet = ''
        content = 'None'
    }
    else
    {
        bullet = paste0("<td style='vertical-align: text-top; font-size: ", font.size, 
                        "'>&#149;&nbsp;&nbsp;</td>")
        tpops = lapply(tpop.codes, target.population.from.code)
        content = sapply(tpops, function(tpop){
            lump.idu.in.name(target.population.name(tpop))})
    }
    #paste0("", paste0("&#149; ", sapply(tpops, target.population.name), collapse='<BR>'), "")
    HTML(paste0("<table>",
                paste0("<tr>",
                       bullet,
                       "<td style='text-align: left; font-style: italic; font-size: ", font.size, "'>",
                       content,
                       "</td></tr>", collapse=''), 
                "</table>"))
}

wrap.radioGroupButtons <- function(buttons)
{
    sub = buttons$children[!sapply(buttons$children, is.null)]
}

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


