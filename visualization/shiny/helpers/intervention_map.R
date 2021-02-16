

create.intervention.map <- function()
{
    list()
}

map.interventions.to.codes <- function(interventions,
                                       map)
{
    if (!is(interventions, 'list'))
        interventions = list(interventions)
    
    sapply(interventions, function(int){
        mask = sapply(map, function(mapped.int){
            interventions.equal(int, mapped.int)
        })
        if (any(mask))
            names(map)[mask][1]
        else
            NA
    })
}

map.codes.to.interventions <- function(codes,
                                       map)
{
    map[codes]
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