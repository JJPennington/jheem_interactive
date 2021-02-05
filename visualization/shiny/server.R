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
#source("R/plot_resources.R")
#source("R/plot_shiny_interface.R")
#source('R/server_helpers.R')
#source('R/styling_helpers.R')
#source('R/server_utils.R')
#source("R/server.routes.docs.R")
#source("R/server.routes.runModel.R")
#source("R/model_code/plot_simulations.R")


##----------------------##
##-- SET UP THE CACHE --##
##----------------------##
shinyOptions(cache=diskCache(file.path(dirname(tempdir()), "myapp-cache")))
# Constants / initiliazers
# TODO: @jef/@tf: Add a 2nd diskCache that is dedicated to the necessary
# datasets we want to lazy load on app start for all sessions. For every,
# city, pulls those 2 files from the diskCache.
# CACHE = memoryCache(size = 20e6)
CACHE = diskCache(max_size = 20e6)

##------------------------------##
##-- THE MAIN SERVER FUNCTION --##
##------------------------------##
server <- function(input, output, session) 
{
    observeEvent(input$location_preset, {
        print(input$location_preset)
    })
}