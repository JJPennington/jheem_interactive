
##--------------------------------------##
##          server_helpers.R            ##
##                                      ##
##   Contains helper functions for the  ##
##   main server.R file                 ##
##--------------------------------------##

# Plot functionality ####
##---------------------------------------------##
##-- THE MAIN PLOT/TABLE GENERATING FUNCTION --##
##        (Plus a convenience wrapper)         ##
##---------------------------------------------##

do.generate.plot.and.table <- function(session,
                                       input,
                                       type=c('prerun','custom')[1],
                                       intervention.settings,
                                       intervention.codes,
                                       web.version,
                                       cache,
                                       intervention.map
                                       )
{
    web.version.data = get.web.version.data(web.version)
    
    if (type=='custom')
    {
        if (is.null(intervention.settings))
            intervention.settings = web.version.data$get.custom.settings.function(input)   
        simset.filter = web.version.data$get.custom.filter.from.settings(intervention.settings)
        apply.filter.function = web.version.data$apply.custom.filter.function
    }
    else
    {
        if (is.null(intervention.settings))
            intervention.settings = web.version.data$get.prerun.settings.function(input)
        simset.filter = web.version.data$get.prerun.filter.from.settings(intervention.settings)
        apply.filter.function = web.version.data$apply.prerun.filter.function
        if (is.null(intervention.codes))
            intervention.codes = web.version.data$get.prerun.intervention.codes.from.settings(intervention.settings)
    }
    
    generate.plot.and.table(session=session,
                            main.settings = get.main.settings(input, type),
                            control.settings = get.control.settings(input, type),
                            intervention.settings=intervention.settings,
                            intervention.codes=intervention.codes,
                            simset.filter=simset.filter,
                            apply.filter.function=apply.filter.function,
                            web.version=web.version,
                            cache=cache,
                            intervention.map=intervention.map
                            )
}

generate.plot.and.table <- function(session,
                                    main.settings,
                                    intervention.settings,
                                    control.settings,
                                    intervention.codes,
                                    simset.filter,
                                    apply.filter.function,
                                    web.version,
                                    cache,
                                    intervention.map=NULL) 
{
    web.version.data = get.web.version.data(web.version)
  
    tryCatch({
        #-- Set up intervention filenames and pull to cache --#
        
        filenames = c(
            Baseline = get.baseline.filename(version=web.version.data$baseline.model.version, 
                                               location=main.settings$location),
            noint = get.simset.filename(version=web.version.data$noint.model.version, 
                                                             location=main.settings$location,
                                                    intervention = NO.INTERVENTION)
            )
        
        if (length(intervention.codes)>0)
            filenames = c(filenames,
                          int = get.simset.filename(intervention.code = intervention.codes,
                                                    version=web.version.data$interventions.model.version, 
                                                    location=main.settings$location)
            )
        
        names(filenames)[names(filenames)=='noint'] = web.version.data$noint.name
        names(filenames)[names(filenames)=='int'] = web.version.data$intervention.name
        
        if (!pull.files.to.cache(session, filenames, cache))
            return (NULL)
        
        #-- Make the plot --# ####
        plot.results = make.simulations.plot.and.table(
            cache=cache,
            location=main.settings$location,
            filenames = filenames,
            simset.filter = simset.filter,
            apply.filter.function = apply.filter.function,
            years=control.settings$years,
            data.types=control.settings$data.types,
            facet.by=control.settings$facet.by,
            split.by=control.settings$split.by,
            dimension.subsets=control.settings$dimension.subsets,
            plot.format=control.settings$plot.format,
            plot.interval.coverage = control.settings$plot.interval.coverage,
            label.change = control.settings$label.change,
            change.years = control.settings$change.years,
            data.type.names = WEB.DATA.TYPE.NAMES,
            change.decrease.is.positive = F,
            baseline.name = 'Baseline',
            noint.name = web.version.data$noint.name,
            post.baseline.year = web.version.data$min.intervention.year,
            change.statistic = control.settings$plot.statistic)
        
        
        #-- Store Settings --#
        plot.results$main.settings = main.settings
        plot.results$control.settings = control.settings
        plot.results$int.settings = intervention.settings
        plot.results$web.version = web.version
        
        #-- Add the intervention --#
        # This is assuming just ONE intervention code for now
        selected.int = NULL
        
        if (!is.null(intervention.codes))
        {
            if (!is.null(intervention.map))
                selected.int = map.codes.to.interventions(intervention.codes[1], intervention.map)[[1]]
            if (is.null(selected.int))
                selected.int = intervention.from.code(intervention.codes[1])
            
            plot.results$intervention = selected.int
        }
        else
            plot.results$intervention = NULL
        
        plot.results$intervention.codes = intervention.codes
        
        
        #-- Return --#
        plot.results
    },
    error = function(e){
        log.error(e)
        show.error.message(session,
                           "Error Generating Figure",
                           "We could not generate the figure and table due to an unexpected error. We apologize. Please let us know if this continues to happen.")
        
        #-- Return NULL --#
        NULL  
    })
    
}

##----------------##
##-- FORMATTING --##
##----------------##

format.plotly.toolbar <- function(plot,
                                  always.visible=F)
{
  # https://plotly.com/r/reference/#layout-updatemenus
  plot = layout(plot,
                modebar=list(
                  orientation='v',
                  borderwidth=1,
                  bordercolor='black',
                  bgcolor='#3c8dbc',#'#f6d8ac',
                  font=list(size=30),
                  position='left',
                  height='30px',
                  color='#f1f7e7',##3c8dbc',
                  activecolor='#b8d585',#'#255876',
                  x=0,
                y=0,
                xanchor='left',
                yanchor='bottom'
                ))
  
  plot = config(plot,
       #         displayModeBar=T,
                displaylogo=F,
                scrollZoom=F,
                
   #             toImageButtonOptions=list(filename=get.default.download.filename(input)),
                
                modeBarButtons=list(
            #      list('toImage'),
                  list('zoom2d'),
                  list('pan2d'),
                  list('zoomIn2d'),
                  list('zoomOut2d'),
                  list('autoScale2d')
                ),
       responsive=T
  )
   #modebar button options at 
  # https://github.com/plotly/plotly.js/blob/master/src/components/modebar/buttons.js
  
  
  plot
}


