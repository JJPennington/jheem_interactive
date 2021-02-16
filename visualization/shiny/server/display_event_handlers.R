
##-----------------------------------------##
##-- EVENT HANDLERS FOR UPDATING DISPLAY --##
##-----------------------------------------##
add.display.event.handlers <- function(session, input, output, cache)
{
    
    #-- Variables for storing plot/table --#
    plot.and.table.list = list(prerun=NULL,
                               custom=NULL)
    
    #-- The Intervention Map for Custom Interventions --#
    custom.int.map = create.intervention.map()

    
    #-- The Handler for Generating/Redrawing Pre-Run --#
    do.prerun = function()
    {   
        #-- Lock the appropriate buttons --#
        lock.cta.buttons(input, called.from.suffix = suffix,
                         plot.and.table.list=plot.and.table.list)
        
        
        plot.and.table.list$prerun <<- generate.plot.and.table(input=input, 
                                                               cache=cache,
                                                               intervention.codes=get.intervention.selection(input, 'prerun'),
                                                               suffix='prerun')
        
        #-- Update the UI --#
        set.display(input, output, 'prerun', plot.and.table.list$prerun)
        sync.buttons.to.plot(input, plot.and.table.list)
        unlock.cta.buttons(input, called.from.suffix = suffix,
                           plot.and.table.list=plot.and.table.list)
    }
    
    observeEvent(input$run_prerun, {
        do.prerun()
        js$chime_if_checked('chime_run_prerun')
    })

    observeEvent(input$redraw_prerun, {
        do.prerun()
    })
    
    
    #-- The Handlers for Generating/Redrawing Custom --#
    
    do.custom = function()
    {
        # Lock the appropriate buttons
        lock.cta.buttons(input, called.from.suffix = suffix,
                         plot.and.table.list=plot.and.table.list)
        
        # Get the selected intervention
        selected.int = get.selected.custom.intervention(input)
        selected.int.code = map.interventions.to.codes(selected.int, custom.int.map)
        
        # Run simulation if needed and store
        if (is.na(selected.int.code))
        {
            version = get.version()
            location = get.selected.location(input, 'custom')
            
            simset = simulate.intervention(version = version,
                                           location = location,
                                           intervention = selected.int,
                                           cache = cache)
            
            selected.int.code = get.new.custom.intervention.code()
            custom.int.map <<- put.intervention.to.map(selected.int.code, selected.int, custom.int.map)
            cache <<- put.simset.to.explicit.cache(get.intervention.filenames(selected.int.code, 
                                                                              version=version, location=location), 
                                                   simset,
                                                   cache=cache, explicit.name = 'custom')
        }
        
        # Make the plot
        plot.and.table.list$custom <<- generate.plot.and.table(input=input, 
                                                               cache=cache,
                                                               intervention.codes=selected.int.code,
                                                               suffix='custom',
                                                               intervention.map = custom.int.map)
        
        # Update the UI
        set.display(input, output, 'custom', plot.and.table.list$custom)
        sync.buttons.to.plot(input, plot.and.table.list)
        unlock.cta.buttons(input, called.from.suffix = suffix,
                           plot.and.table.list=plot.and.table.list)
    }
    
    observeEvent(input$run_custom, {
        do.custom()
        js$chime_if_checked('chime_run_custom')
    })
    
    
    observeEvent(input$redraw_custom, {
        do.custom()
    })
    
    #-- The Handler for Locations --#
    observeEvent(input$location_prerun, {
        plot.and.table.list['prerun'] <<- list(NULL)
        clear.display(input, output, 'prerun')
        sync.buttons.to.plot(input, plot.and.table.list)
    })
    
    observeEvent(input$location_prerun, {
        #I'm not sure we want to do the same thing as before
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