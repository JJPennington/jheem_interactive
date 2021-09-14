
##----------------------------------------##
##--           filters.R                --##
##--                                    --##
##-- Code to filter (thin) simsets by   --##
##-- parameter values or other criteria --##
##----------------------------------------##


# A parameter filter is a named list
# names correspond to parameter names
# each element of this list is a numeric vector of length 2 [lower bound, upper bound]


get.filter.mask <- function(parameters.or.simset, parameter.filter)
{
    if (is(parameters.or.simset, 'simset'))
        parameters.or.simset = parameters.or.simset@parameters
    
    parameter.names = intersect(names(parameter.filter), 
                                dimnames(parameters.or.simset)[[2]])
    
    if (length(parameter.names)==0)
        rep(T, dim(parameters.or.simset)[1])
    else
    {
        mask.by.parameter = 
            sapply(parameter.names, function(pp.name){
                parameters.or.simset[,pp.name] >= parameter.filter[[pp.name]][1] &
                    parameters.or.simset[,pp.name] <= parameter.filter[[pp.name]][2]
            })
        
        apply(mask.by.parameter, 1, all)
    }
}


filter.simset.by.parameters <- function(simset, 
                                        simset.name,
                                        filter)
{
    if (is.null(filter))
        simset
    else
    {
        mask = get.filter.mask(parameters.or.simset=simset, parameter.filter=filter)
        if (any(!mask))
            subset.simset(simset, mask)
        else
            simset
    }
}