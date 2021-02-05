
#'@description Get the locations for which simulations are available for a
#' given model version
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@return A named character vector. The values of the vector are 'display
#'able' names of places; the names of the vector are the location codes to
#' be passed to plot.simulations
get.preset.locations <- function(version)
{
    sims.names = sims.list()
    locations = get.locations.from.filenames(sims.names)
    locations = unique(locations)
    location.names = unlist(msa.names(locations))
    names(location.names) = locations
    location.names = sort(location.names)
    
    location.names
}


##------------------------------------------##
##-- GETTING INTERVENTIONS FROM THE CLOUD --##
##------------------------------------------##

#returns a list with two elements
# $location - a vector of location ids
# $intervention - a list of interventions
get.interventions.list <- function(include.no.intervention=F)
{
    sims.names = sims.list()
    locations = get.locations.from.filenames(sims.names)
    
    interventions = get.interventions.from.filenames(sims.names)
    
    if (!include.no.intervention)
    {
        mask = !sapply(interventions, is.null.intervention)
        interventions = interventions[mask]
        locations = locations[mask]
    }
    
    o = order.interventions(interventions)
    interventions = interventions[o]
    locations = locations[o]
    
    list(location=locations,
         intervention=interventions,
         code = sapply(interventions, get.intervention.code))
}

extract.unique.interventions.from.list <- function(interventions)
{
    codes = unique(interventions$code)
    lapply(codes, intervention.from.code)
}

##-------------##
##-- HELPERS --##
##-------------##

