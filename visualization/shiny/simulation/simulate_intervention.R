

simulate.intervention <- function(version,
                                  location,
                                  intervention,
                                  cache)
{
    baseline.filename = get.seed.filename(location=location,
                                          version=version)

    
    
    seed.simset = get.simsets.from.cache(filenames, cache)
    
}

get.custom.intervention <- function(input)
{
    subpopulation.nums = 1:get.custom.n.subpopulations(input)
    target.populations = lapply(subpopulation.nums,
                                get.custom.subpopulation, input=input)
    
    unit.interventions = lapply(subpopulation.nums,
                                get.custom.unit.interventions, input=input)
    
    sub.interventions = lapply(subpopulation.nums, function(i){
        create.intervention(target.populations[[i]], unit.interventions[[i]])
    })
    
    join.interventions(sub.interventions)
}



##-- MID-LEVEL --##

get.custom.subpopulation <- function(input, num)
{
    create.target.population()
}

get.custom.unit.intervention <- function(input, num)
{
    
}

get.custom.intervention.code <- function()
{
    # Strip the time of non-numeric numbers
    time.marker = Sys.time()
    time.marker = gsub("[^1-9 ]", '', time.marker)
    time.marker = trimws(time.marker)
    time.marker = gsub(' ', '_', time.marker)
    
    time.marker
    
    # Generate a random string
    random.str.len = 4
    chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    
    random.indices = ceiling(runif(random.str.len, .0000001, nchar(chars)))
    random.str = paste0(sapply(random.indices, function(i){
        substr(chars, i, i)
    }), collapse='')
    
    # Put them together
    paste0(time.marker, '_', random.str)
}

##-- GETTERS --##

get.custom.n.subpopulations <- function(input)
{
    
}

get.custom.ages <- function(input, num)
{
    
}

get.custom.races <- function(input, num)
{
    
}

get.custom.sexes <- function(input, num)
{
    
}

get.custom.risks <- function(input, num)
{
    
}