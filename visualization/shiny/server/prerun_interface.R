
#'@description Get the locations for which simulations are available for a
#' given model version
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@return A named character vector. The values of the vector are 'display
#'able' names of places; the names of the vector are the location codes to
#' be passed to plot.simulations
get.prerun.locations <- function(version)
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

get.prerun.intervention.codes <- function()
{
    sims.names = sims.list()
    locations = get.locations.from.filenames(sims.names)
    
    intervention.codes = get.intervention.codes.from.filenames(sims.names)
    
    list(intervention.code=intervention.codes,
         location=locations)
}

get.baseline.filename <- function(version,
                                  location)
{
    get.simset.filename(version=version,
                        location=location,
                        intervention=NULL)
}

NO.INTERVENTION.CODE = get.intervention.code(NO.INTERVENTION)
get.no.intervention.filename <- function(version,
                                         location)
{
    get.intervention.filenames(NO.INTERVENTION.CODE,
                               version,
                               location)
}

get.intervention.filenames <- function(codes,
                                       version,
                                       location)
{ 
    sapply(codes, function(code){
        get.simset.filename(location = location,
                            intervention.code = code)
    })
}
