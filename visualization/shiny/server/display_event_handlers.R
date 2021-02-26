
##-----------------------------------------##
##-- EVENT HANDLERS FOR UPDATING DISPLAY --##
##-----------------------------------------##
add.display.event.handlers <- function(session, input, output, cache,
                                       suffixes = c('prerun','custom'))
{
    
    #-- Variables for storing plot/table --#
    plot.and.table.list = lapply(suffixes, function(s){NULL})
    names(plot.and.table.list) = suffixes
    is.first.plot = T
    
    #-- The Intervention Map for Custom Interventions --#
    custom.int.map = create.intervention.map()

    
    #-- Resize Listener --#
    
    handle.resize <- function(suffixes)
    {
        lapply(suffixes, function(suffix){
            if (!is.null(plot.and.table.list[[suffix]]))
                set.display(input=input,
                            output=output,
                            suffix=suffix,
                            plot.and.table = plot.and.table.list[[suffix]])
        })
    }
    
    observeEvent(input$display_size, handle.resize(names(plot.and.table.list)) )
    observeEvent(input$left_width_prerun, handle.resize('prerun') )
    observeEvent(input$right_width_prerun, handle.resize('prerun') )
    observeEvent(input$left_width_custom, handle.resize('custom') )
    observeEvent(input$right_width_custom, handle.resize('custom') )
    
    #-- General Handler for Running/Redrawing --#
    do.run = function(intervention.codes, suffix)
    {   
        get.display.size(input, 'prerun')
        
        #-- Lock the appropriate buttons --#
        lock.cta.buttons(input, called.from.suffix = suffix,
                         plot.and.table.list=plot.and.table.list)
        
        if (check.plot.controls(session, input, suffix))
        {
            new.plot.and.table = generate.plot.and.table(main.settings = get.main.settings(input, suffix),
                                                         control.settings = get.control.settings(input, suffix),
                                                         intervention.codes=intervention.codes,
                                                         cache=cache,
                                                         intervention.map = custom.int.map)
            
            if (!is.null(new.plot.and.table))
            {
                plot.and.table.list[[suffix]] <<- new.plot.and.table
                
                #-- Update the UI --#
                set.display(input, output, suffix, plot.and.table.list[[suffix]])
                sync.buttons.to.plot(input, plot.and.table.list)
            }
        }
        
        unlock.cta.buttons(input, called.from.suffix = suffix,
                           plot.and.table.list=plot.and.table.list)
        
        # Expand the control panel if this is the first plot we have generated
        if (is.first.plot)
        {
            is.first.plot <<- F
            js$trigger_accordion('prerun_expand_right')
        }
    }
    
    #-- The Handlers for Generating/Redrawing Pre-Run --#
    observeEvent(input$run_prerun, {
  #    progress.test(session)
  #    return()
      
        do.run(intervention.codes = get.intervention.selection(input, 'prerun'),
               suffix='prerun')
        js$chime_if_checked('chime_run_prerun')
    })

    observeEvent(input$redraw_prerun, {
        do.run(intervention.codes=plot.and.table.list$prerun$intervention.codes,
               suffix='prerun')
    })
    
    
    #-- The Handlers for Generating/Redrawing Custom --#
    
    observeEvent(input$run_custom, {
        
        if (check.custom.inputs(session, input))
        {
            # Lock the appropriate buttons
            lock.cta.buttons(input, called.from.suffix = suffix,
                             plot.and.table.list=plot.and.table.list)
            
            # Get the selected intervention
            selected.int = get.selected.custom.intervention(input)
            selected.int.code = map.interventions.to.codes(selected.int, custom.int.map)
            
            #set.intervention.panel(output, 'custom', selected.int)
            
            # Run simulation if needed and store
            if (is.na(selected.int.code))
            {
                version = get.version()
                location = get.selected.location(input, 'custom')
                
                simset = simulate.intervention(version = version,
                                               location = location,
                                               intervention = selected.int,
                                               cache = cache)
                
                if (is.null(simset))
                {
                    unlock.cta.buttons(input, called.from.suffix = suffix,
                                       plot.and.table.list=plot.and.table.list)   
                    js$chime_if_checked('chime_run_custom')
                    return()
                }
                else                
                {
                      selected.int.code = get.new.custom.intervention.code()
                      custom.int.map <<- put.intervention.to.map(selected.int.code, selected.int, custom.int.map)
                      cache <<- put.simset.to.explicit.cache(get.intervention.filenames(selected.int.code, 
                                                                                        version=version, location=location), 
                                                             simset,
                                                             cache=cache, explicit.name = 'custom')
                }
            }
            
            do.run(intervention.codes = selected.int.code,
                   suffix='custom')
            
            # Keep only the simsets we need
            custom.int.map <<- thin.map(keep.codes = plot.and.table.list$custom$intervention.codes,
                                        map = custom.int.map)
            
            intervention.filenames = get.intervention.filenames(plot.and.table.list$custom$intervention.codes,
                                                                version=plot.and.table.list$custom$main.settings$version, 
                                                                location=plot.and.table.list$custom$main.settings$location)
            cache <<- thin.explicit.cache(keep.codes = intervention.filenames,
                                          cache = cache,
                                          explicit.name = 'custom')
            
            # Play the chime
            js$chime_if_checked('chime_run_custom')
        } else {
            # @Todd: I put error messages inside `error_checking.R`
        }
    })
    
    
    observeEvent(input$redraw_custom, {
        do.run(intervention.codes = plot.and.table.list$custom$intervention.codes,
               suffix='custom')
    })
    
    #-- The Handler for Locations --#
    observeEvent(input$location_prerun, {
        plot.and.table.list['prerun'] <<- list(NULL)
        clear.display(input, output, 'prerun')
        sync.buttons.to.plot(input, plot.and.table.list)
    })
    
    observeEvent(input$location_custom, {
        plot.and.table.list['custom'] <<- list(NULL)
        clear.display(input, output, 'custom')
        sync.buttons.to.plot(input, plot.and.table.list)
    })
    
    #-- Share Handlers --#
 
    lapply(names(plot.and.table.list), function(suffix){
      
        #-- Figure --#
        observeEvent(input[[paste0('download_figure_', suffix)]], {
            download.plot(plot.and.table.list[[suffix]], suffix=suffix)
        })
        
        #-- Table --#
        output[[paste0('download_table_', suffix)]] = downloadHandler(
            filename = function() {get.default.download.filename(plot.and.table.list[[suffix]], ext='csv')},
            content = function(filepath) {
                write.csv(plot.and.table.list[[suffix]]$change.df, filepath) 
            }
        )
        
        observeEvent(input[[paste0('share_link_', suffix)]], {
            print(paste0("Share ", suffix, ' link'))
        })
    })
    
    # Get the size on load
    session$onFlushed(function(){
        js$ping_display_size()
        
        js$set_input_value(name='left_width_prerun', value=as.numeric(LEFT.PANEL.SIZE['prerun']))
        js$set_input_value(name='right_width_prerun', value=0)
        js$set_input_value(name='left_width_custom', value=as.numeric(LEFT.PANEL.SIZE['custom']))
        js$set_input_value(name='right_width_custom', value=0)
        
        lapply(names(plot.and.table.list), 
               clear.display,
               input=input,
               output=output)
        
        sync.buttons.to.plot(input, plot.and.table.list)
    }, once=T)
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
        set.run.button.enabled(input, suffix, F)
        set.redraw.button.enabled(input, suffix, F)
        set.share.enabled(input, suffix, F)
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