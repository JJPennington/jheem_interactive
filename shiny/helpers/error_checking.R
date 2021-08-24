# Checks, for each subgroup
# 1. That it is not empty
#   ie, at least one value checked for each age, race, sex, and risk factor
# 2. if only 'female' is checked, then at least one non-msm category is checked 
# 3. That there is at least one intervention (on testing, PrEP, or suppression)
# 
# If valid, return true
#  else pop up a dialog and return false
check.custom.inputs <- function(session, input)
{
    errors = get.custom.errors(input)
    if (length(errors)==0)
        T
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
        
        show.warning.message(session,
                           title='Intervention Not Fully Specified:',
                           message = msg)
        
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




get.custom.errors <- function(input)
{
    errors = lapply(1:get.custom.n.subpopulations(input), function(i){
        
        sub.errors = sapply(DIMENSION.VALUES.2, function(dimension){
            
            selection = do.get.custom.tpop.selection(input, i, dim=dimension)
            if (length(selection)>0)
                as.character(NA)
            else
                paste0("You must specify at least one ",
                       tolower(dimension$label))
        })
        sub.errors = sub.errors[!is.na(sub.errors)]
        
        if (all(names(sub.errors)!='sex') &&
            all(names(sub.errors)!='risk') &&
            all(get.custom.sexes(input, i) != 'male') &
            any(grepl('msm', get.custom.risks(input, i))))
            sub.errors = c(sub.errors,
                           "If you include MSM as a risk factor, you must include male sex")
        
        if (!get.custom.use.testing(input, i) &&
            !get.custom.use.prep(input, i) &&
            !get.custom.use.suppression(input, i) &&
            !get.custom.use.needle.exchange(input, i) &&
            !get.custom.use.moud(input, i))
            sub.errors = c(sub.errors,
                           "You must specify at least one intervention component (HIV Testing, PrEP, Viral Suppression, Needle Exchange, or MOUDs)")
        
        names(sub.errors) = NULL
        sub.errors
    })
    names(errors) = 1:get.custom.n.subpopulations(input)
    
    errors = errors[sapply(errors, function(sub){length(sub)>0})]
    errors
}