##---------------------------------------##
##--            analytics.R            --##
##--
##--    Code for tracking analytics
##--    We track one data point for every button click on a call-to-action button
##---------------------------------------##


##--------------------##
##-- SOME CONSTANTS --##
##--------------------##

ANALYTICS.DELIMITER = ';'
ANALYTICS.BUCKET = 'endinghiv.analytics'


##-----------------------------------------##
##--        THE MAIN FUNCTION            --##
##-- (a wrapper around the actual do-er) --##
##-----------------------------------------##

# Has innate error handling and fails silently
track.request <- function(session.id,
                          suffix,
                          called.from,
                          web.version.data,
                          main.settings,
                          intervention.settings,
                          control.settings,
                          intervention.codes,
                          intervention,
                          query.string,
                          track.local.requests = F
                          )
{
    tryCatch({
        is.local = Sys.getenv('SHINY_PORT') == ''
        
        if (is.local && !track.local.requests)
            print("NOT RECORDING ANALYTICS SINCE WE'RE ON A LOCAL MACHINE")
        else
        {
            if (is.local) #some tracking if we're in development mode
            {
                start.time = Sys.time()
                print('doing analytics')
            }
            
            # Aggregate tracking data not directly from settings or intervention 
            if (is.null(intervention.codes))
                intervention.codes = NA
            else
                intervention.codes = paste0(intervention.codes, collapse=ANALYTICS.DELIMITER)
            
            header = list(session.id = session.id,
                          created_at = Sys.time(),
                          category = suffix,
                          called.from = called.from,
                          web.version = web.version.data$name,
                          location = main.settings$location,
                          intervention.codes=intervention.codes,
                          query.string=query.string,
                          testing=is.local)
            
            if (is.null(intervention))
                intervention.trackable = list()
            else
                intervention.trackable = web.version.data$intervention.to.trackable.function(intervention)

            to.track = c(header,
                         control.settings.to.trackable(control.settings, web.version.data),
                         web.version.data$prerun.settings.to.trackable.function(intervention.settings),
                         intervention.trackable
                         )
            
            do.track.request(to.track)
            
            if (is.local) #just for performance eval while were testing locally
            {
                print(paste0('done with analytics: ',
                             difftime(start.time, Sys.time(), units='secs'),
                             ' sec'))
            }
        }
    },
    error=function(e){
        if (is.local)
            stop(e)
        else
            log.error(e)
    })
}

##----------------------------------------##
##--   THE FUNCTION TO ACTUALLY TRACK   --##
##-- (pushes a data frame to Amazon S3) --##
##--            plus helpers            --##
##----------------------------------------##

# track.elems is a named list of values to track
# the method=DB is an old holdover for using database (with external dependencies)
do.track.request <- function(track.elems,
                             method=c('S3', 'DB')[1])
{
    track.elems[sapply(track.elems, is.null)] = NA
    df = as.data.frame(track.elems)
    names(df) = gsub('\\.','_',names(df))


    # For now, just put it to S3
    if (method == 'S3') 
    {
        filename = get.analytics.filename()
        s3save(df, object=filename, bucket=ANALYTICS.BUCKET)          
    } 
    else
        db.write.rows(
            table.name='analytics',
            data.df=df)
}

# gets a unique filename
# basically, we're just trusting in the 36^5 combinations of letters + digits
#   to assume that we don't generate the same filename twice in one day
get.analytics.filename <- function()
{
    # generate a random string
    len=5
    choices = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
    indices = ceiling(runif(len, .000001, nchar(choices)))
    chars = sapply(indices, function(i){substr(choices, i, i)})
    rand.str = paste0(chars, collapse='')
        
    # put the string in a directory path corresponding to the date
    file.path(gsub('-', '/', Sys.Date()),
              paste0(rand.str, '.Rdata'))
}


##---------------------------------------------------------------##
##-- PULL TRACKING INFORMATION FROM DIFFERENT SETTINGS OBJECTS --##
##---------------------------------------------------------------##


intervention.settings.to.trackable <- function(settings, suffix,
                                               web.version.data)
{
    if (suffix=='custom')
        list()
    else
        web.version.data$prerun.intervention.settings.to.trackable(settings)
}

ehe.intervention.to.trackable <- function(intervention)
{
    tpops = get.unique.target.populations(intervention)
    
    ages.by.tpop = sapply(tpops, FUN=apply, 'age', any)
    ages = dimnames(ages.by.tpop)[[1]][apply(ages.by.tpop, 1, any)]
    ages = paste0(ages, collapse=ANALYTICS.DELIMITER)
    
    races.by.tpop = sapply(tpops, FUN=apply, 'race', any)
    races = dimnames(races.by.tpop)[[1]][apply(races.by.tpop, 1, any)]
    races = paste0(races, collapse=ANALYTICS.DELIMITER)
    
    sexes.by.tpop = sapply(tpops, FUN=apply, 'sex', any)
    sexes = dimnames(sexes.by.tpop)[[1]][apply(sexes.by.tpop, 1, any)]
    sexes = paste0(sexes, collapse=ANALYTICS.DELIMITER)
    
    risks.by.tpop = sapply(tpops, FUN=apply, 'risk', any)
    risks = dimnames(risks.by.tpop)[[1]][apply(risks.by.tpop, 1, any)]
    risks = paste0(risks, collapse=ANALYTICS.DELIMITER)
    
    
    intervention.start = min(unlist(sapply(intervention$raw, function(sub){
        sapply(sub$intervention.units, function(unit){unit$start.year})
    })))
    
    intervention.end = max(unlist(sapply(intervention$raw, function(sub){
        sapply(sub$intervention.units, function(unit){unit$years})
    })))
    
    if (is.null(intervention$raw$testing))
        testing = NA
    else
        testing = paste0(unlist(sapply(intervention$raw$testing$intervention.units, function(unit){
            unit$rates
        })), collapse=ANALYTICS.DELIMITER)
    
    if (is.null(intervention$raw$prep))
        prep = NA
    else
        prep = paste0(unlist(sapply(intervention$raw$prep$intervention.units, function(unit){
            unit$rates
        })), collapse=ANALYTICS.DELIMITER)
    
    if (is.null(intervention$raw$suppression))
        suppression = NA
    else
        suppression = paste0(unlist(sapply(intervention$raw$suppression$intervention.units, function(unit){
            unit$rates
        })), collapse=ANALYTICS.DELIMITER)
    
    if (is.null(intervention$raw$needle.exchange))
        needle.exchange = NA
    else
        needle.exchange = paste0(unlist(sapply(intervention$raw$needle.exchange$intervention.units, function(unit){
            unit$rates
        })), collapse=ANALYTICS.DELIMITER)
    
    # MOUDs
    if (is.null(intervention$raw$idu.relapse))
        moud = NA
    else
        moud = paste0(unlist(sapply(intervention$raw$idu.relapse$intervention.units, function(unit){
            unit$raw.rates
        })), collapse=ANALYTICS.DELIMITER)
    
    
    list(
        n.target.subgroups = length(tpops),
        intervention.start = intervention.start,
        intervention.end = intervention.end,
        target.ages = ages,
        target.races = races,
        target.sexes = sexes,
        target.risks = sexes, 
        testing = testing,
        prep = prep,
        viral.suppression = suppression,
        needle.exchange = needle.exchange,
        moud = moud
    )
}

covid.prerun.settings.to.trackable <- function(settings)
{
    rv = list()
    for (pp.name in names(settings$filter))
    {
        rv[[paste0(pp.name,'.lower')]] = settings$filter[[pp.name]][1]
        rv[[paste0(pp.name,'.upper')]] = settings$filter[[pp.name]][2]
    }
    
    rv
}

covid.intervention.to.trackable <- function(intervention)
{
    if (!is(intervention, 'covid.intervention'))
        return(list())
    
    ages.by.tpop = sapply(intervention$sub.populations, FUN=apply, 'age', any)
    ages = dimnames(ages.by.tpop)[[1]][apply(ages.by.tpop, 1, any)]
    ages = paste0(ages, collapse=ANALYTICS.DELIMITER)
    
    races.by.tpop = sapply(intervention$sub.populations, FUN=apply, 'race', any)
    races = dimnames(races.by.tpop)[[1]][apply(races.by.tpop, 1, any)]
    races = paste0(races, collapse=ANALYTICS.DELIMITER)
    
    sexes.by.tpop = sapply(intervention$sub.populations, FUN=apply, 'sex', any)
    sexes = dimnames(sexes.by.tpop)[[1]][apply(sexes.by.tpop, 1, any)]
    sexes = paste0(sexes, collapse=ANALYTICS.DELIMITER)
    
    risks.by.tpop = sapply(intervention$sub.populations, FUN=apply, 'risk', any)
    risks = dimnames(risks.by.tpop)[[1]][apply(risks.by.tpop, 1, any)]
    risks = paste0(risks, collapse=ANALYTICS.DELIMITER)
    
    
    intervention.start = min(unlist(sapply(intervention$sub.units, function(sub){
        sub$start.time
    })))
    
    intervention.span = mean(unlist(sapply(intervention$sub.units, function(sub){
        sapply(sub$parameters, function(pp){
            if (pp$use)
                pp$normalize.span
            else
                NA
        })
    })), na.rm=T)
    
    intervention.end = max(unlist(sapply(intervention$sub.units, function(sub){
        sapply(sub$parameters, function(pp){
            if (pp$use)
                pp$start.normalize.time + pp$normalize.span/12
            else
                -Inf
        })
    })))
    
    
    rv = list(
        n.target.subgroups = intervention$rv$n.subpopulations,
        intervention.start = intervention.start,
        intervention.end = intervention.end,
        target.ages = ages,
        target.races = races,
        target.sexes = sexes,
        target.risks = sexes
    )
    
    
    for (pp in COVID.PARAMETERS)
    {
        lower = min(sapply(intervention$sub.units, function(sub){
            sub.param = sub$parameters[[pp$name]]
            
            if (sub.param$use)
                sub.param$effect[1]
            else
                NA
        }), na.rm=T)
        if (is.infinite(lower))
            lower = NA
        rv[[paste0(pp$name,'.lower')]] = lower
        
        upper = max(sapply(intervention$sub.units, function(sub){
            sub.param = sub$parameters[[pp$name]]
            
            if (sub.param$use)
                sub.param$effect[2]
            else
                NA
        }), na.rm=T)
        if (is.infinite(upper))
            upper = NA
        rv[[paste0(pp$name,'.upper')]] = upper
    }
    
    rv
}