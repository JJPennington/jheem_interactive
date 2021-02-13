
##-------------------##
##-- LIBRARY CALLS --##
##-------------------##
#library(processx)
#library(orca)
library(shiny)
library(mailR)
library(cachem)


##------------------##
##-- SOURCE FILES --##
##------------------##

source('env.R')
source('plot_interface/generate_plot.R')
source('server/display_event_handlers.R')
source('server/control_helpers.R')
source('plot_interface/generate_plot.R')
source('plot_interface/plot_interface.R')
source('server/multi_cache.R')
#source('load_resources.R')
#source('ui/display_helpers.R', local=T)
source('ui/custom_interventions.R', local=T)


##----------------------##
##-- SET UP THE CACHE --##
##----------------------##
#shinyOptions(cache=diskCache(file.path(dirname(tempdir()), "myapp-cache")))
DISK.CACHE.1 = cachem::cache_disk(max_size = 1e9, evict='lru')
DISK.CACHE.2 = cachem::cache_disk(max_size = 1e9, evict='lru')

##---------------------------##
##-- THE server() FUNCTION --##
##---------------------------##

server <- function(input, output, session, cache) 
{
    ##--------------------##    
    ##-- INITIAL SET-UP --##
    ##--------------------##
    
    query.string = 'hi' 
    #@joe fill in here
    
    # Print an initial message - useful for debugging on shinyapps.io servers
    print(paste0("Launching server() function - ", Sys.time()))
    
    # Make our session cache
    mem.cache = cachem::cache_mem(max_size = 60e6, evict='lru')
    cache = create.multi.cache(mem.cache=mem.cache, disk.caches=list(DISK.CACHE.1, DISK.CACHE.2))
    
    
    ##-----------------------------------------##
    ##-- EVENT HANDLERS FOR UPDATING DISPLAY --##
    ##-----------------------------------------##

    # in server/display_event_handlers.R
    add.display.event.handlers(session, input, output, cache)
    
    observeEvent(input$testlink, {
        print('test link')
    })
    
    ##-----------------------------------------------##
    ##-- EVENT HANDLERS FOR UPDATING PLOT CONTROLS --##
    ##-----------------------------------------------##
    
    # in server/control_helpers.R
    add.control.event.handlers(session, input, output, cache, suffix='prerun')
    
    ##-----------------------------------------------##
    ##-- EVENT HANDLERS: CUSTOM INTERVENTIONS      --##
    ##-----------------------------------------------##
    max_subpops = 5
    # Custom interventions: Left panel tabPanel
    observeEvent(input$n_subpops, {
        for (i in 1:max_subpops)
          if (i <= as.integer(input$n_subpops))
            showTab(inputId="subpop_tabset_panel", target=as.character(i))
          else
            hideTab(inputId="subpop_tabset_panel", target=as.character(i))
    })
    
    # Custom interventions: Left panel demographic checkbox groups
    dimension.value.options = get.dimension.value.options(
        version=version,
        location=input$geographic_location,
        msm_idu_mode=TRUE)
    # state = reactiveVal(list())  # Will probably move this up.
    state = list()
    for (i in 1:max_subpops)
        lapply(dimension.value.options, function(dim) {
            # Increment toggle count
            # - prevents auto-check-all on app load, while presevering check-all
            #   on toggle off functionality
            checkboxGroup.switchId = paste0(dim[['name']], '_switch', i)
            checkboxGroup.boxesId = paste0(dim[['name']], i)
            key = paste0(checkboxGroup.boxesId, '.toggleCount')
            # state2 = state()
            state2 = state
            
            # if (key %in% names(state2))
            #     state2[[key]] = state2[[key]] + 1
            # else
            #     state2[[key]] = 0
            state2[[key]] = -1
            
            # state(state2)
            state <<- state2
            observeEvent(input[[checkboxGroup.switchId]], {
                # state2 = state()
                state2 = state
                
                # if (key %in% names(state2))
                #     state2[[key]] = state2[[key]] + 1
                # else
                #     state2[[key]] = 0
                state2[[key]] = state2[[key]] + 1
                
                # state(state2)
                state <<- state2
                if (input[[checkboxGroup.switchId]] == TRUE)
                    shinyjs::hide(id=checkboxGroup.boxesId)
                else {
                    shinyjs::show(id=checkboxGroup.boxesId)
                    # browser()
                    if (state2[[key]] > 0)
                        updateCheckboxGroupInput(
                            inputId=checkboxGroup.boxesId,
                            selected=names(dim[['choices']]))
                }
            })
        })  # </lapply/"/for">
}
