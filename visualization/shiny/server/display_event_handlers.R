
##-----------------------------------------##
##-- EVENT HANDLERS FOR UPDATING DISPLAY --##
##-----------------------------------------##
add.display.event.handlers <- function(session, input, output, cache)
{
    
    observeEvent(input$location_prerun, {
        
        plot.and.table.prerun <<- NULL
        clear.display(input, output, 'prerun')
        
        if (1==2 && !is.null(input$location_prerun))
        {
            print(paste0("pre-pulling from cache for ", input$location_prerun))
            pull.simsets.to.cache(get.sim.filenames.to.load(get.version(), 
                                                            location=input$location_prerun,
                                                            intervention.codes = get.intervention.code(NO.INTERVENTION)),
                                  cache)
            print(paste0("done pre-pulling from cache for location", input$location_prerun))
        }
    })
    
    observeEvent(input$run_prerun, {
        plot.and.table.prerun <<- generate.plot.and.table(input, cache, 'prerun')
        #plot.and.table.prerun <<- list(table=NULL, plot=make.plotly.message())
        set.display(input, output, 'prerun', plot.and.table.prerun)
    })
    
    output$table_holder_prerun = renderText(
        paste0(input$location_prerun, " ", Sys.time())
    )
}