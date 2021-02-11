
##-------------------##
##-- LIBRARY CALLS --##
##-------------------##
library(processx)
#library(orca)
library(shiny)
library(mailR)


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
shinyOptions(cache=diskCache(file.path(dirname(tempdir()), "myapp-cache")))
# Constants / initiliazers
# TODO: @jef/@tf: Add a 2nd diskCache that is dedicated to the necessary
# datasets we want to lazy load on app start for all sessions. For every,
# city, pulls those 2 files from the diskCache.
# CACHE = memoryCache(size = 20e6)
CACHE = diskCache(max_size = 1e6)

DISK.CACHE.1 = diskCache(max_size = 1e9, evict='lru')
DISK.CACHE.2 = diskCache(max_size = 1e9, evict='lru')

##---------------------------##
##-- THE server() FUNCTION --##
##---------------------------##

server <- function(input, output, session, cache) 
{
    ##--------------------##    
    ##-- INITIAL SET-UP --##
    ##--------------------##
    
    # Print an initial message - useful for debugging on shinyapps.io servers
    print(paste0("Launching server() function - ", Sys.time()))
    
    # Make our session cache
    mem.cache = memoryCache(max_size = 60e6, evict='lru')
    cache = create.multi.cache(mem.cache=mem.cache, disk.caches=list(DISK.CACHE.1, DISK.CACHE.2))
    
    # Variables for storing plot/table
    plot.and.table.prerun = NULL
    plot.and.table.custom = NULL
    
    # Pages
    # output$custom_content = get.custom_content(input)
    
    ##-----------------------------------------##
    ##-- EVENT HANDLERS FOR UPDATING DISPLAY --##
    ##-----------------------------------------##

    # in server/display_event_handlers.R
    add.display.event.handlers(session, input, output, cache)
    
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
    for (i in 1:max_subpops)
        lapply(dimension.value.options, function(dim) {
            group.switchId = paste0(dim[['name']], '_switch', i)
            group.contentId = paste0(dim[['name']], i)
            observeEvent(input[[group.switchId]], {
                if (input[[group.switchId]] == TRUE)
                    shinyjs::hide(id=group.contentId)
                else
                    shinyjs::show(id=group.contentId)
            })
        })
    
}
