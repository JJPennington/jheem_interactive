
##-----------------------------------------##
##-- EVENT HANDLERS FOR UPDATING DISPLAY --##
##-----------------------------------------##
add.display.event.handlers <- function(session, input, output, cache,
                                       suffixes = c('prerun','custom'))
{
    #-- Variables for storing plot/table --#
    plot.and.table.list = lapply(suffixes, function(s){NULL})
    names(plot.and.table.list) = suffixes
    links = lapply(suffixes, function(s){NULL})
    names(links) = suffixes
    is.first.plot = T
    
    #-- Make a random session id --#
    # (for analytics)
    choices = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    n.char.id = 20
    set.seed(NULL)
    session.id = paste0(sapply(ceiling(runif(n.char.id, 0.00001, nchar(choices))), function(index){
        substr(choices, index, index)
    }), collapse='')
    
    #-- The Intervention Map for Custom Interventions --#
    custom.int.map = create.intervention.map()
    
    
    #-- Outputs --#
    observe({
        setup.outputs = get.web.version.data(get.web.version(input))$setup.outputs
        if (!is.null(setup.outputs))
            setup.outputs(input, output)
    })
    
    
    #-- General Handler for Running/Redrawing --#
    do.run = function(intervention.codes, 
                      suffix, 
                      intervention.settings,
                      called.from,
                      chime.if.id=NULL,
                      thin.custom.cache = F)
    {   
        get.display.size(input, 'prerun')
        
        #-- Lock the appropriate buttons --#
        lock.cta.buttons(input, called.from.suffix = suffix,
                         plot.and.table.list=plot.and.table.list)
        
        need.to.track = F
        if (check.plot.controls(session, input, suffix))
        {
            web.version = get.web.version(input)

            #Generate a promise instead of a value

            #Only continue processing when the future is finished.
            #The external options used to be outside of the check.plot.controls if statement;
            #I have simply included it in the then() condition for the promise.

            new.plot.and.table.prom = future_promise(do.generate.plot.and.table(session=session,
                                                            input=input,
                                                            type=suffix,
                                                            intervention.codes=intervention.codes,
                                                            intervention.settings=intervention.settings,
                                                            web.version=web.version,
                                                            cache=cache,
                                                            intervention.map=custom.int.map),
                                                     seed=TRUE)

            then(new.plot.and.table.prom,
                 function(new.plot.and.table) {
                     if (!is.null(new.plot.and.table))
                     {
                        plot.and.table.list[[suffix]] <<- new.plot.and.table
                        links[[suffix]] <<- NULL
                
                        #-- Update the UI --#
                        set.display(input, output, suffix, plot.and.table.list[[suffix]],
                                    web.version.data = get.web.version.data(web.version))
                        sync.buttons.to.plot(input, plot.and.table.list)
                
                        need.to.track=T
                        unlock.cta.buttons(input, called.from.suffix = suffix,
                                           plot.and.table.list=plot.and.table.list)

                        # Expand the control panel if this is the first plot we have generated
                        if (is.first.plot)
                        {
                            is.first.plot <<- F
                            js$trigger_accordion('prerun_expand_right')
                        }

                        # Remove saved custom interventions, if needed
                        if (thin.custom.cache)
                        {
                            custom.int.map <<- thin.map(keep.codes =
                                                        plot.and.table.list$custom$intervention.codes,
                                                        map = custom.int.map)
                            
                            intervention.filenames = get.intervention.filenames(
                                                      plot.and.table.list$custom$intervention.codes,
                                                      version=plot.and.table.list$custom$main.settings$version,
                                                      location=plot.and.table.list$custom$main.settings$location)

                            cache <<- thin.explicit.cache(keep.codes = intervention.filenames,
                                                          cache = cache,
                                                          explicit.name = 'custom')
                        }

                        # Chime (if applicable)
                        if (!is.null(chime.if.id))
                            js$chime_if_checked(chime.if.id)

                        # Track with analytics
                        if (!is.null(new.plot.and.table))
                        {
                            track.request(session.id=session.id,
                                          suffix=suffix,
                                          called.from=called.from,
                                          web.version.data=get.web.version.data(get.web.version(input)),
                                          main.settings=new.plot.and.table$main.settings,
                                          intervention.settings=new.plot.and.table$int.settings,
                                          control.settings=new.plot.and.table$control.settings,
                                          intervention.codes=new.plot.and.table$intervention.codes,
                                          intervention=new.plot.and.table$intervention,
                                          query.string=session$clientData$url_search
                                          )
                        }
                    }
                 })
        }
    }
    
    #-- The Handlers for Generating/Redrawing Pre-Run --#
    observeEvent(input$run_prerun, {
        #    progress.test(session)
        #    return()
        
        web.version.data = get.web.version.data(get.web.version(input))
        int.settings = web.version.data$get.prerun.settings.function(input)
        
        if (check.prerun.errors(session=session,
                                settings=int.settings,
                                web.version.data=web.version.data))
        {
            do.run(intervention.codes = NULL, 
                   intervention.settings=int.settings,
                   called.from = 'run_prerun', suffix='prerun',
                   chime.if.id = 'chime_run_prerun')
        }
    })
    
    observeEvent(input$redraw_prerun, {
        do.run(intervention.codes=plot.and.table.list$prerun$intervention.codes,
               intervention.settings=plot.and.table.list$prerun$int.settings,
               called.from = 'redraw_prerun', suffix='prerun')
    })
    
    
    #-- The Handlers for Generating/Redrawing Custom --#
    
    observeEvent(input$run_custom, {
        
        # Get the settings
        web.version.data = get.web.version.data(get.web.version(input))
        int.settings = get.custom.settings(input, web.version.data=web.version.data)
        
        # Check for errors
        if (check.custom.errors(session=session,
                                settings=int.settings,
                                web.version.data = web.version.data))
        {
            # Lock the appropriate buttons
            lock.cta.buttons(input, called.from.suffix = suffix,
                             plot.and.table.list=plot.and.table.list)
          
            # Get the selected intervention
            selected.int = web.version.data$get.custom.intervention.from.settings(int.settings)
            #            selected.int = get.selected.custom.intervention(input)
            #           int.settings = get.custom.settings(input)
            selected.int.code = map.interventions.to.codes(selected.int, custom.int.map,
                                                           web.version.data=web.version.data)
            
            #set.intervention.panel(output, 'custom', selected.int)
            
            # Run simulation if needed and store
            if (is.na(selected.int.code))
            {
                location = get.selected.location(input, 'custom')
                
                simset = simulate.intervention(session,
                                               web.version.data = web.version.data,
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
                    
                    print(paste0("custom code is '", selected.int.code, "'"))
                    custom.int.map <<- put.intervention.to.map(selected.int.code, selected.int, custom.int.map)
                    cache <<- put.simset.to.explicit.cache(get.simset.filename(intervention.code = selected.int.code,
                                                                               version=web.version.data$interventions.model.version, 
                                                                               location=location),
                                                           simset,
                                                           cache=cache, explicit.name = 'custom')
                }
            }
            
            do.run(intervention.codes = selected.int.code,
                   called.from = 'run_custom',
                   suffix='custom',
                   intervention.settings = int.settings,
                   chime.if.id = 'chime_run_custom',
                   thin.custom.cache = T)
        }
    })
    
    
    observeEvent(input$redraw_custom, {
        do.run(intervention.codes = plot.and.table.list$custom$intervention.codes,
               called.from = 'redraw_custom',
               suffix='custom',
               intervention.settings = plot.and.table.list$custom$int.settings)
    })
    
    #-- The Handler for Locations --#
    observeEvent(input$location_prerun, {
        plot.and.table.list['prerun'] <<- list(NULL)
        links['prerun'] <<- list(NULL)
        clear.display(input, output, 'prerun')
        sync.buttons.to.plot(input, plot.and.table.list)
    })
    
    observeEvent(input$location_custom, {
        plot.and.table.list['custom'] <<- list(NULL)
        links['custom'] <<- list(NULL)
        clear.display(input, output, 'custom')
        sync.buttons.to.plot(input, plot.and.table.list)
    })
    
    #-- Share Handlers --#
    
    lapply(names(plot.and.table.list), function(suffix){
        
        #-- Figure --#
        
        # this handler launches the modal to get width and height
        observeEvent(input[[paste0('download_figure_', suffix)]], {
            if (!is.null(plot.and.table.list[[suffix]]))
                show.download.plot.modal(session=session,
                                         plot.and.table=plot.and.table.list[[suffix]], 
                                         input=input,
                                         suffix=suffix)
        })
        
        # this handler processes the 'Download' click from the modal
        observeEvent(input[[paste0('do_download_figure_', suffix)]], {
            removeModal()
            do.download.plot(plot.and.table=plot.and.table.list[[suffix]], 
                             suffix=suffix,
                             width=input[[paste0('download_figure_width_', suffix)]],
                             height=input[[paste0('download_figure_height_', suffix)]])
        })
        
        #-- Table --#
        output[[paste0('download_table_', suffix)]] = downloadHandler(
            filename = function() {get.default.download.filename(plot.and.table.list[[suffix]], ext='csv')},
            content = function(filepath) {
                write.csv(plot.and.table.list[[suffix]]$change.df, filepath) 
            }
        )
        
        observeEvent(input[[paste0('share_link_', suffix)]], {
            if (!is.null(plot.and.table.list[[suffix]]))
            {
                if (is.null(links[[suffix]]))
                    links[[suffix]] <<- save.link(session, 
                                                  plot.and.table=plot.and.table.list[[suffix]], 
                                                  suffix=suffix, 
                                                  cache=cache)
                
                if (!is.null(links[[suffix]]))
                    show.link.message(session, links[[suffix]])
            }
        })
    })
    
    #-- Some Initial Set-Up Once Loaded --#
    
    session$onFlushed(function(){
        js$ping_display_size_onload()
        
        js$set_input_value(name='left_width_prerun', value=as.numeric(LEFT.PANEL.SIZE['prerun']))
        js$set_input_value(name='right_width_prerun', value=0)
        js$set_input_value(name='left_width_custom', value=as.numeric(LEFT.PANEL.SIZE['custom']))
        js$set_input_value(name='right_width_custom', value=0)
        
        lapply(names(plot.and.table.list), 
               clear.display,
               input=input,
               output=output)
        
        # Sync up
        sync.buttons.to.plot(input, plot.and.table.list)
        
    }, once=T)
    
    
    #-- Process the query string --#
    
    # Listeners to render if loading a preset
    
    render.from.link <- function()
    {   
        is.first.plot <<- F # So we don't pop out the sidebar controls
        
        int.code = initial.link.data$intervention.codes
        
        do.run(intervention.codes = int.code, 
               suffix=initial.link.data$type, 
               called.from = 'link',
               intervention.settings = initial.link.data$intervention.settings)
        links[[initial.link.data$type]] <<- initial.link
        js$chime_if_checked(paste0('chime_run_', initial.link.data$type))
        
        initial.link.data <<- NULL
        initial.link <<- NULL
    }
    
    #-- Resize Listener --#
    
    handle.resize <- function(suffixes)
    {
        lapply(suffixes, function(suffix){
            display.size = get.display.size(input, suffix)
            if (length(display.size$width)>0 && length(display.size$height)>0)
            {
                if (!is.null(plot.and.table.list[[suffix]]))
                {
                    set.display(input=input,
                                output=output,
                                suffix=suffix,
                                plot.and.table = plot.and.table.list[[suffix]],
                                web.version.data = get.web.version.data(get.web.version(input)))
                }
                else # We check here for render from a link, because we don't want to 
                    #  render before display size is synced up
                {
                    if (!is.null(initial.link.data) && initial.link.data$type == suffix)
                        render.from.link()
                    
                }
            }
        })
    }
    
    observeEvent(input$display_size_prerun, handle.resize('prerun') )
    observeEvent(input$display_size_custom, handle.resize('custom') )
    observeEvent(input$left_width_prerun, handle.resize('prerun') )
    observeEvent(input$right_width_prerun, handle.resize('prerun') )
    observeEvent(input$left_width_custom, handle.resize('custom') )
    observeEvent(input$right_width_custom, handle.resize('custom') )
    
    observeEvent(input$main_nav, {
        js$ping_display_size() 
    })
    
    # The observe handler that sets things in motion if we need to set up a link
    initial.link = NULL
    initial.link.data = NULL
    observe({
        
        # Check for a query string to kick us off
        query.string = session$clientData$url_search
        query.settings = parse.query.settings(query.string)
        
        if (!is.null(query.settings$link.data))
        {
            link.data = query.settings$link.data
            
            set.main.to.settings(session, suffix=link.data$type, link.data$main.settings)
            set.controls.to.settings(session, input, suffix=link.data$type, link.data$control.settings)
            
            web.version.data = get.web.version.data(get.web.version(input))        
            if (link.data$type=='custom')
            {
                set.custom.to.settings(session,
                                       input,
                                       link.data$intervention.settings,
                                       web.version.data=web.version.data)
                #                set.custom.to.settings(session,
                #                                      input,
                #                                     settings = link.data$intervention.settings)
            }
            else
            {
                set.intervention.selection(session, 
                                           suffix='prerun', 
                                           int.code=link.data$intervention.codes)
            }
            
            initial.link <<- query.settings$link.query
            initial.link.data <<- link.data 
            
            updateNavbarPage(session,
                             inputId = 'main_nav',
                             selected = paste0(initial.link.data$type, "_interventions"))
        }
    })
    
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
