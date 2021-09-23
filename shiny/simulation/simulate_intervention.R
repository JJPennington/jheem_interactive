
MAX.KEEP.FROM.YEAR = 2018

simulate.intervention <- function(session,
                                  web.version.data,
                                  location,
                                  intervention,
                                  cache)
{
    seed.filename = get.seed.filename(location=location,
                                          version=web.version.data$seed.model.version)
    
    success = pull.files.to.cache(session, seed.filename, cache)
    if (!success)
        return(NULL) 
    
    tryCatch({
        
        seed.simset = get.simsets.from.cache(seed.filename, cache)[[1]]
        
    #THIS IS FOR TESTING - in the local environment
        is.local = Sys.getenv('SHINY_PORT') == ''
        if (is.local)
        {
            print("Using limited seed for now")
            seed.simset = subset.simset(seed.simset, 1:5)
        }

        print ("Step")

        withProgress(
            message=paste0("Preparing to run ", seed.simset@n.sim, " simulations"), 
            min=0, max=seed.simset@n.sim, value=0,
            detail=NULL,
            {   
                print ("Step")
                run.from.year = attr(seed.simset, 'run.from.year')
                keep.from.year = min(run.from.year-1, MAX.KEEP.FROM.YEAR-1)
                
                start.time = Sys.time()
                simset = web.version.data$simulate.function(simset = seed.simset, 
                                                 intervention = intervention,
                                                 run.from.year = run.from.year,
                                                 run.to.year = web.version.data$run.simulations.to.year,
                                                 keep.years = keep.from.year:web.version.data$run.simulations.to.year,
                                                 
                                                 update.progress=function(i){
                                                     time.diff = as.numeric(difftime(Sys.time(), start.time, units='secs'))
                                                     time.text = get.timespan.text(time.diff,
                                                                                   digits.for.last.span = 0)
                                                     setProgress(value=i,
                                                                 message=paste0("Running Simulation ", i, " of ", seed.simset@n.sim, ": "),
                                                                 detail=paste0(time.text, " elapsed"))
                                                 })
                setProgress(seed.simset@n.sim, detail='Done')
            })

        compress.simset(simset)
    },
    error = function(e){
        show.error.message("Error Simulating Intervention",
                           "There was an error running the simulations for the specified intervention. We apologize - please try again later.")
        NULL
    })
    print ("OUT")
}

get.ehe.custom.intervention.from.settings <- function(settings)
{
    subpopulation.nums = 1:settings$n.subpopulations
    target.populations = lapply(subpopulation.nums,
                                get.custom.subpopulation, settings=settings)
    
    unit.interventions = lapply(subpopulation.nums,
                                get.custom.unit.interventions, settings=settings)
    
    sub.interventions = lapply(subpopulation.nums, function(i){
        create.intervention(target.populations[[i]], unit.interventions[[i]])
    })
    
    join.interventions(sub.interventions)
}


##-- MID-LEVEL --##

get.custom.subpopulation <- function(settings, num)
{
    ages = settings$sub.populations[[num]]$age #get.custom.ages(input, num)
    races = settings$sub.populations[[num]]$race #get.custom.races(input, num)
    sexes = settings$sub.populations[[num]]$sex #get.custom.sexes(input, num)
    risks = settings$sub.populations[[num]]$risk #get.custom.risks(input, num)
    
    tpop = NULL
    iterated.sexes = rep(sexes, each=length(risks))
    iterated.risks = rep(risks, length(sexes))
    mask = iterated.sexes != 'female' | !grepl('msm', iterated.risks)
    iterated.sexes = iterated.sexes[mask]
    iterated.risks = iterated.risks[mask]
    
    for (i in 1:length(iterated.sexes))
    {
        if (iterated.sexes[i]=='female')
            new.sex = 'female'
        else if (grepl('msm', iterated.risks[i]))
            new.sex = 'msm'
        else
            new.sex = 'heterosexual_male'
        
        if (grepl('active', iterated.risks[i]))
            new.risk = 'active_IDU'
        else if (grepl('prior', iterated.risks[i]))
            new.risk = 'IDU_in_remission'
        else
            new.risk = 'never_IDU'
        
        new.tpop = create.target.population(ages=ages,
                                            races=races,
                                            sexes=new.sex,
                                            risks=new.risk)
        
        if (is.null(tpop))
            tpop = new.tpop
        else
            tpop = union.target.populations(tpop, new.tpop)
    }
    
    tpop
}

get.custom.unit.interventions <- function(settings, num)
{
    start.year = settings$sub.units[[num]]$start.year # get.custom.start.year(input, num)
    end.year = settings$sub.units[[num]]$end.year # get.custom.end.year(input, num)
    
    rv = list()
    
    #-- Testing --#
    if (settings$sub.units[[num]]$use.testing) # (get.custom.use.testing(input, num))
    {
        rv = c(rv,
               list(create.intervention.unit(type = 'testing', 
                                             start.year = start.year, 
                                             rates = 12/settings$sub.units[[num]]$testing.frequency, #get.custom.testing.frequency(input, num),
                                             years = end.year)
               ))
    }
    
    #-- PrEP --#
    if (settings$sub.units[[num]]$use.prep) #(get.custom.use.prep(input, num))
    {
        rv = c(rv,
               list(create.intervention.unit(type = 'prep', 
                                             start.year = start.year, 
                                             rates = settings$sub.units[[num]]$prep.uptake, #get.custom.prep.uptake(input, num),
                                             years = end.year)
               ))
    }
    
    #-- Suppression --#
    if (settings$sub.units[[num]]$use.suppression) #(get.custom.use.suppression(input, num))
    {
        rv = c(rv,
               list(create.intervention.unit(type = 'suppression', 
                                             start.year = start.year, 
                                             rates = settings$sub.units[[num]]$suppressed.proportion, #get.custom.suppressed.proportion(input, num),
                                             years = end.year)
               ))
    }
    
    #-- Needle Exchange --#
    if (settings$sub.units[[num]]$use.needle.exchange) #(get.custom.use.needle.exchange(input, num))
    {
        rv = c(rv,
               list(create.intervention.unit(type = 'needle.exchange', 
                                             start.year = start.year, 
                                             rates = settings$sub.units[[num]]$needle.exchange.proportion, #get.custom.needle.exchange.proportion(input, num),
                                             years = end.year)
               ))
    }
    
    #-- MOUDs --#
    if (settings$sub.units[[num]]$use.moud) #(get.custom.use.moud(input, num))
    {
        rv = c(rv,
               list(create.moud.intervention(start.year=start.year,
                                             coverages=settings$sub.units[[num]]$moud.proportion, #get.custom.moud.proportion(input, num),
                                             years = end.year)
               ))
    }    
    rv
}
