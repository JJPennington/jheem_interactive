
WEB.DATA.TYPE.NAMES = OUTCOME.OPTIONS$names
names(WEB.DATA.TYPE.NAMES) = OUTCOME.OPTIONS$values

# Main Function: THE PLOT FUNCTION ####
#'@param description The function that actually generates plots
#'
#' THE FUNDAMENTAL ARGUMENTS THAT DEFINE THE PLOT
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@param intervention.name
#'@param years The numeric years to plot
#'@param data.types The names of 
#'@param facet.by [char[]] The names of dimensions according to which to
#' 'facet' (a separate panel for each) - this should be a subset of name
#' s(get.facet.by.options(location))
#'@param split.by [char[]] The names of dimensions according which to
#' split the plot (a separate line on each panel) - this should be a
#'  subset of names(get.split.by.options(location))
#'@param dimension.subsets [type] A named list of which values for each
#' dimension to include in the plot. The names of the list are the named
#' dimensions as given by names(get.dimension.value.options()), and the
#'  values for dimension d must be a subset of get.dimension.values()[[d]]
#'@param plot.format [type] The character string indicating the format for
#' the plot - this should be one of 
#' names(get.plot.format.options(location))
#'@param show.truth [type] Whether to include "truth" (known from epi
#' surveillance) in the plot
#'
#' ARGUMENTS THAT DEFINE STATISTICS
#'@param plot.interval.coverage A fraction (0 to 1) which the plotted
#' prediction interval should cover (only applies to some plot formats)
#'@param summary.statistic The name of the summary statistic to show.
#' Should be one of names(get.summary.statistic.options(location))
#'@param summary.statistic.interval.coverage A fraction (0 to 1) which the
#' interval for the summary statistic should cover
#'
#' THE STYLE ARGUMENTS
#'@param baseline.color The color with which to plots the baseline 
#'(pre-intervention) simulations
#'@param truth.color The color with which to plot truth (when available)
#'@param intervention.colors A named vector of colors, one for each
#' intervention in intervention.names. The names of the vector should be
#'intervention.names
#'@param plot.interval.alpha The alpha value (opacity) for prediction
#' intervals (if shown)
#'@param simulation.alpha The alpha value (opacity) for individual
#' simulation plots
#'@param simulation.line.size The line size for plotted simulations
#'@param truth.point.size The point size for plotted 'truth' (epi
#' surveillance) values#'
#'
#'
#'@return A list with three values:
#' $plot - a plotly object
#' $change.df - a data frame
#' $notes - a character vector (which may be empty) of notes
make.simulations.plot.and.table <- function(
    cache,
    # Private meta params
    version,
    # Public params; Selectable in UI
    location,
    filenames,
    years,
    data.types,
    data.type.names,
    facet.by,
    split.by,
    dimension.subsets,  # TODO: Problem? all 4 vals are null
    plot.format,
    
    label.change=T,
    change.years=c(2020,2030),
    change.decrease.is.positive=F,
    
    show.truth=T,
    plot.interval.coverage=0.95,
    
    plot.interval.alpha=0.25,
    simulation.alpha=0.125,
    simulation.line.size=if (plot.format=='individual.simulations') 2 else 5,
    truth.point.size=10,
    
    ncol=NULL
) {
    withProgress(min=0, max=1, value = 0, 
                 message="Building Figure and Table...", {
        
        print(paste0("In mem cache: ", 
                     paste0(cache$mem.cache$keys(), collapse=', ')))
        simsets = get.simsets.from.cache(filenames, cache)
        names(simsets) = names(filenames)
        
        #Figure out coloring
        if (length(simsets)<=2 && length(split.by)>0)
            color.by = 'split'
        else
            color.by = 'intervention'
    
        rv = do.plot.simulations(
            return.plot.components = T,
            
            simsets,
            years=years,
            data.types=data.types,
            facet.by,
            split.by,
            dimension.subsets,
            #for now, going to override the plot formats
            plot.format=plot.format, 
            
            show.truth=T,
            
            plot.interval.coverage=plot.interval.coverage,
            #summary.statistic=summary.statistic,
            #summary.statistic.interval.coverage=summary.statistic.interval.coverage,
            
            colors=pal_jama(),
            
            plot.interval.alpha=plot.interval.alpha,
            simulation.alpha=simulation.alpha,
            simulation.line.size=simulation.line.size,
            truth.point.size=truth.point.size,
            
            color.by = color.by,
            
            label.change = label.change,
            change.years = change.years,
            change.decrease.is.positive = change.decrease.is.positive,
            
            progress.update = setProgress,
            
            data.type.names = DATA.TYPE.NAMES,
            return.change.data.frame = T,
            ncol = ncol,
            
            place.labels.to.side = T,
            wrap.axis.labels = T,
            vline.change.years = T,
            
            margin.top=30
            )
        
        
            setProgress(value=1)
        })
    
    rv$notes = c('test note 1', 'test note 2')
    rv
}
