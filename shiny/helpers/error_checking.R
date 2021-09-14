# Checks, for each subgroup
# 1. That it is not empty
#   ie, at least one value checked for each age, race, sex, and risk factor
# 2. if only 'female' is checked, then at least one non-msm category is checked 
# 3. That there is at least one intervention (on testing, PrEP, or suppression)
# 
# If valid, return true
#  else pop up a dialog and return false
check.custom.errors <- function(session, settings, web.version.data)
{
    errors = get.custom.errors(settings, web.version.data=web.version.data)
    errors = aggregate.errors.to.message(errors)
    
    if (is.null(errors))
        T
    else
    {
        show.warning.message(session,
                             title='Intervention Not Fully Specified:',
                             message = errors)
        
        F
    }
}

check.prerun.errors <- function(session, settings, web.version.data)
{
    errors = web.version.data$check.prerun.errors(settings)
    if (length(errors)==0)
        T
    else
    {
     #   errors = tagAppendAttributes(errors,
         #                            class='errors')
        
        show.warning.message(session,
                             title=paste0('Invalid ', web.version.data$intervention.label, ' Settings:'),
                             message = errors)
        
        F
    }
        
}


# 1. Check that at least one outcome is checked
# If not, pop up dialog and return false
# 
# 2. Multiply the number of selected outcomes by the number of options in each 
# of the selected facet by dimensions
# If greater than MAX.ADVISED.N.PANELS, pop up a dialog that says "This is going
#  to generate X panels" are you sure you want to do this
# -return true if 'yes'
# -return false if 'no'
check.plot.controls <- function(session, input, suffix)
{
    #make sure there's at least one
    selected.outcomes = get.selected.outcomes(input, suffix)
    valid = length(selected.outcomes) >= 1
    
    if (!valid)
    {
      outcome.string = paste0(paste0(OUTCOME.OPTIONS$names[-length(OUTCOME.OPTIONS$names)], collapse=', '),
                              ", or ",
                              OUTCOME.OPTIONS$names[length(OUTCOME.OPTIONS$names)])
      show.warning.message(session,
                         title='Invalid Outcome Selection', 
                         paste0('At least one outcome (', outcome.string, ') must be checked.'))
    }
    
    return(valid)
 }


error.check.custom.ehe.intervention.unit <- function(settings, i)
{
    unit = settings$sub.units[[i]]
    if (!unit$use.testing &&
        !unit$use.prep &&
        !unit$use.suppression &&
        !unit$use.needle.exchange &&
        !unit$use.moud)
    {
        "You must specify at least one intervention component (HIV Testing, PrEP, Viral Suppression, Needle Exchange, or MOUDs)"
    }
    else
        NULL
}


get.custom.errors <- function(settings, web.version.data)
{
    errors = lapply(1:settings$n.subpopulations, function(i){
        sub.pop = settings$sub.populations[[i]]
        
        sub.errors = sapply(DIMENSION.VALUES.2, function(dimension){
            
            selection = sub.pop[[dimension$code]]
            #selection = do.get.custom.tpop.selection(input, i, dim=dimension)
            if (length(selection)>0)
                as.character(NA)
            else
                paste0("You must specify at least one ",
                       tolower(dimension$label))
        })
        sub.errors = sub.errors[!is.na(sub.errors)]
        
        if (all(names(sub.errors)!='sex') &&
            all(names(sub.errors)!='risk') &&
            all(sub.pop$sex != 'male') &
            any(grepl('msm', sub.pop$risk)))
            sub.errors = c(sub.errors,
                           "If you include MSM as a risk factor, you must include male sex")
        
        unit.errors = web.version.data$check.custom.unit.errors(settings, i)
        if (!is.null(unit.errors))
            sub.errors = c(sub.errors, unit.errors)
        
        names(sub.errors) = NULL
        sub.errors
    })
    names(errors) = 1:settings$n.subpopulations
    
    errors = errors[sapply(errors, function(sub){length(sub)>0})]
    errors
}


# A helper that aggregates different errors into one message
# errors should be a list with one element for each subpopulation
# each element is a vector of one or more errors
aggregate.errors.to.message <- function(errors)
{
    if (length(errors)==0)
        NULL
    else
    {
        msg = do.call(tags$div, lapply(names(errors), function(i){
            sub.errors = errors[[i]]
            names(sub.errors) = NULL
            
            errors.list = do.call(tags$ul, lapply(sub.errors, tags$li))
            tags$div(tags$h4(paste0("Subgroup ", i, ":")),
                     errors.list)
        }))
        
        msg = tagAppendAttributes(msg,
                                  class='errors')
        
        msg
    } 
}