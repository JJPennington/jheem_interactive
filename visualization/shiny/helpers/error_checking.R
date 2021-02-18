

# Checks, for each subgroup
# - That it is not empty
#   ie, at least one value checked for each age, race, sex, and risk factor
#       AND, if only 'female' is checked, then at least one non-msm category is checked 
# - That there is at least one intervention (on testing, PrEP, or suppression)
# 
# If valid, return true
#  else pop up a dialog and return false
check.custom.inputs <- function(session, input)
{
    return (TRUE) #for now
    for (i in 1:get.custom.n.subpopulations(input))
    {
        get.custom.ages(input, i)
        
        if (!get.custom.use.testing(input, i) && !get.custom.use.prep(input, i) && !get.custom.use.suppression(input, i))
            print('not allowed')
            
    }
}

MAX.ADVISED.N.PANELS = 8

# Check that at least one outcome is checked
# If not, pop up dialog and return false
# 
# Multiply the number of selected outcomes by the number of options in each of the selected facet by dimensions
# If greater than MAX.ADVISED.N.PANELS, pop up a dialog that says "This is going to generate X panels" are you sure you want to do this
# -return true if 'yes'
# -return false if 'no'
# 
check.plot.controls <- function(session, input, suffix)
{
    return (TRUE) # For now
    
    selected.outcomes = get.selected.outcomes(input, suffix)
    #make sure there's at least one
    
    get.num.panels.to.plot(input, suffix)
    #if more than MAX.ADVISED.N.PANELS, alert
}


