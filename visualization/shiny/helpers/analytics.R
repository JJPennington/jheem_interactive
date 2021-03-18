

ANALYTICS.DELIMITER = ';'
ANALYTICS.BUCKET = 'endinghiv.analytics'
track.request <- function(session.id,
                          called.from,
                          main.settings,
                          control.settings,
                          intervention.code,
                          intervention
                          )
{
    is.local = Sys.getenv('SHINY_PORT') == ''
    
    if (is.local)
        print("NOT RECORDING ANALYTICS SINCE WE'RE ON A LOCAL MACHINE")
    else
    {
#        start.time = Sys.time()
 #       print('doing analytics')
        
        if (is.null(intervention) || is.null.intervention(intervention))
        {
            tpops = list()
            ages = races = sexes = risks = ''
            intervention.start = intervention.end = NA
            testing = prep = suppression = NA
        }
        else
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
        }    
    
        do.track.request(session.id = session.id,
                         called.from = called.from,
                         version = main.settings$version,
                         location = main.settings$location,
                         intervention.code = intervention.code,
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
                         outcomes = paste0(control.settings$data.types, collapse=ANALYTICS.DELIMITER),
                         facet.by = paste0(control.settings$facet.by, collapse=ANALYTICS.DELIMITER),
                         split.by = paste0(control.settings$split.by, collapse=ANALYTICS.DELIMITER),
                         change.outcome.start = control.settings$change.years[1],
                         change.outcome.end = control.settings$change.years[2],
                         show.change = control.settings$label.change,
                         plot.format = control.settings$plot.format,
                         show.ages = paste0(control.settings$dimension.subsets$age, collapse=ANALYTICS.DELIMITER),
                         show.races = paste0(control.settings$dimension.subsets$race, collapse=ANALYTICS.DELIMITER),
                         show.sexes = paste0(control.settings$dimension.subsets$sex, collapse=ANALYTICS.DELIMITER),
                         show.risks = paste0(control.settings$dimension.subsets$risk, collapse=ANALYTICS.DELIMITER))
        
    
#        print(paste0('done with analytics: ',
 #                    difftime(start.time, Sys.time(), units='secs'),
  #                   ' sec'))
    }
}

# Functions ####
# This should have innate error handling and fail silently
# The database needs to track all these, plus the date/time
do.track.request <- function(method=c('S3', 'DB')[1],
                             session.id, #non-unique string
                             called.from, #string
                             version, #string
                             location, #string
                             intervention.code, #string
                             n.target.subgroups, #integer
                             intervention.start, #numeric
                             intervention.end, #numeric
                             target.ages, #string
                             target.races, #string
                             target.sexes, #string
                             target.risks, #string
                             testing, #may be NA
                             prep, #may be NA
                             viral.suppression, #may be NA
                             outcomes, #string
                             facet.by, #string
                             split.by, #string
                             change.outcome.start, #numeric
                             change.outcome.end, #numeric
                             show.change, #numeric
                             plot.format, #string
                             show.ages, #string
                             show.races, #string
                             show.sexes, #string
                             show.risks #string
)
{
    tryCatch({
        df = data.frame(
            created_at=Sys.time(),
            session_id=session.id,
            called_from=called.from,
            version=version,
            location=location,
            intervention_code=intervention.code,
            n_target_subgroups=n.target.subgroups,
            intervention_start=intervention.start,
            intervention_end=intervention.end,
            target_ages=target.ages,
            target_races=target.races,
            target_sexes=target.sexes,
            target_risks=target.risks,
            testing=testing,
            prep=prep,
            viral_suppression=viral.suppression,
            outcomes=outcomes,
            facet_by=facet.by,
            split_by=split.by,
            change_outcome_start=change.outcome.start,
            change_outcome_end=change.outcome.end,
            show_change=show.change,
            plot_format=plot.format,
            show_ages=show.ages,
            show_races=show.races,
            show_sexes=show.sexes,
            show_risks=show.risks)
   
        # For now, just put it to S3
        if (method == 'S3') {
          filename = get.analytics.filename()
          s3save(data, object=filename, bucket=ANALYTICS.BUCKET)          
        } else
         db.write.rows(
          table.name='analytics',
           data.df=df)
    },
    error=function(e){
        log.error(e)
    })
}

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

# Testing ####
if (1==2)
{
do.track.request(
    method='DB',
    session.id='xxx', #non-unique string
    called.from='xxx', #string
    version='xxx', #string
    location='xxx', #string
    intervention.code='xxx', #string
    n.target.subgroups=3, #integer
    intervention.start=1.0, #numeric
    intervention.end=1.0, #numeric
    target.ages='xxx', #string
    target.races='xxx', #string
    target.sexes='xxx', #string
    target.risks='xxx', #string
    testing='', #may be NA
    prep=NA, #may be NA
    viral.suppression=NA, #may be NA
    outcomes='xxx', #string
    facet.by='xxx', #string
    split.by='xxx', #string
    change.outcome.start=1.5, #numeric
    change.outcome.end=999.8492, #numeric
    show.change=1.0, #numeric
    plot.format='xxx', #string
    show.ages='xxx', #string
    show.races='xxx', #string
    show.sexes='xxx', #string
    show.risks='xxx' #string
  )
}
