
##--------------------------------------##
##          server_helpers.R            ##
##                                      ##
##   Contains helper functions for the  ##
##   main server.R file                 ##
##--------------------------------------##

# Plot functionality ####
##---------------------------------------------##
##-- THE MAIN PLOT/TABLE GENERATING FUNCTION --##
##---------------------------------------------##

generate.plot.and.table <- function(session,
                                    main.settings,
                                    control.settings,
                                    intervention.codes,
                                    intervention.settings=NULL,
                                    cache, 
                                    intervention.map=NULL) 
{
    tryCatch({
        #-- Set up intervention filenames and pull to cache --#
        
        filenames = c(
            'Baseline' = get.baseline.filename(version=main.settings$version, location=main.settings$location),
            'No Intervention' = get.no.intervention.filename(version=main.settings$version, location=main.settings$location),
            'Intervention' = get.intervention.filenames(intervention.codes,
                                                        version=main.settings$version, location=main.settings$location)
        )
        
        if (!pull.files.to.cache(session, filenames, cache))
            return (NULL)
        
        #-- Make the plot --# ####
        plot.results = make.simulations.plot.and.table(
            cache=cache,
            version=main.settings$version,
            location=main.settings$location,
            filenames = filenames,
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
            change.decrease.is.positive = F)
        
        
        
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
        
        plot.results$intervention.codes = intervention.codes
        
        #-- Store Settings --#
        
        plot.results$main.settings = main.settings
        plot.results$control.settings = control.settings
        plot.results$intervention.settings = intervention.settings
        
        
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


