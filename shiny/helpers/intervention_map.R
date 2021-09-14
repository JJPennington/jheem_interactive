

create.intervention.map <- function()
{
    list()
}

map.interventions.to.codes <- function(interventions,
                                       map,
                                       web.version.data)
{
    if (!is(interventions, 'list'))
        interventions = list(interventions)
    
    sapply(interventions, function(int){
        mask = sapply(map, function(mapped.int){
            web.version.data$interventions.equal(int, mapped.int)
        })
        if (any(mask))
            names(map)[mask][1]
        else
            NA
    })
}

web.tool.interventions.equal <- function(int1, int2)
{
    if (is(int1, 'intervention') && is(int2, 'intervention'))
        interventions.equal(int1, int2)
    else
        F
}

map.codes.to.interventions <- function(codes,
                                       map)
{
    lapply(codes, function(code){
        map[[code]]  
    })
}

put.intervention.to.map <- function(code,
                                    intervention,
                                    map)
{
    map[code] = list(intervention)
    map
}

remove.intervention.from.map <- function(code,
                                          map)
{
    map[code] = NULL
    map
}

thin.map <- function(keep.codes, map)
{
    keep.codes = intersect(keep.codes, names(map))
    map = map[keep.codes]
    map
}