
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
}



