
##-----------------------------------------##
##-- EVENT HANDLERS FOR UPDATING DISPLAY --##
##-----------------------------------------##
add.display.event.handlers <- function(session, input, output, cache)
{
    
    # Variables for storing plot/table
    plot.and.table.list = list(prerun=NULL,
                               custom=NULL)

    
    #-- The Handler for Generating Pre-Run --#
    observeEvent(input$run_prerun, {
        plot.and.table.list$prerun <<- generate.plot.and.table(input=input, 
                                                               cache=cache,
                                                               suffix='prerun',
                                                               plot.and.table.list=plot.and.table.list)
        set.display(input, output, 'prerun', plot.and.table.list$prerun)
        sync.buttons.to.plot(input, plot.and.table.list)
    })

    #-- The Handler for Redrawing Pre-Run --#
    observeEvent(input$redraw_prerun, {
        plot.and.table.list$prerun <<- generate.plot.and.table(input=input, 
                                                               cache=cache,
                                                               suffix='prerun',
                                                               plot.and.table.list=plot.and.table.list)
        set.display(input, output, 'prerun', plot.and.table.list$prerun)
        sync.buttons.to.plot(input, plot.and.table.list)
    })
        
    #-- The Handler for Locations --#
    observeEvent(input$location_prerun, {
        plot.and.table.list['prerun'] <<- list(NULL)
        clear.display(input, output, 'prerun')
        sync.buttons.to.plot(input, plot.and.table.list)
    })
    
    #-- Share Handlers --#
}


##-- ENABLING AND DISABLING --##

sync.buttons.to.plot <- function(input, plot.and.table.list)
{
    for (suffix in names(plot.and.table.list))
    {
        enable = !is.null(plot.and.table.list[[suffix]])
        
        set.redraw.button.enabled(input, suffix, enable)
        set.share.enabled(input, suffix, enable)
    }
}


lock.cta.buttons <- function(input,
                             called.from.suffix,
                             plot.and.table.list)
{   
    for (suffix in names(plot.and.table.list))
    {
#        if (suffix != called.from.suffix)
            set.run.button.enabled(input, suffix, F)
        
        set.redraw.button.enabled(input, suffix, F)
    }
}

unlock.cta.buttons <- function(input,
                               called.from.suffix,
                               plot.and.table.list)
{   
    for (suffix in names(plot.and.table.list))
    {
        set.run.button.enabled(input, suffix, T)
    }
    
    sync.buttons.to.plot(input, plot.and.table.list)
}