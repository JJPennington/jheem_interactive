

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
    for (i in 1:get.custom.n.subpopulations(input))
    {
        get.custom.ages(input, i)
        
        if (!get.custom.use.testing(input, i) && !get.custom.use.prep(input, i) && !get.custom.use.suppression(input, i))
            print('not allowed')
            
    }
}


# Check that at least one outcome is checked
# If not, pop up dialog and return false
# 
# Multiply the number of selected outcomes by the number of options in each of the selected facet by dimensions
# If greater than 8, pop up a dialog that says "This is going to generate X panels" are you sure you want to do this
# -return true if 'yes'
# -return false if 'no'
# 
check.plot.controls <- function(session, input, suffix)
{
    selected.outcomes = get.selected.outcomes(input, suffix)
    n.selected.outcomes = length(selected.outcomes)
    
    facet.by = get.selected.facet.by(input, suffix)
    n.facet = prod(sapply(facet.by, function(ff){
        length(DIMENSION.VALUES.2[[ff]]$values)
    }))
    
    if (n.selected.outcomes * n.facet > 8)
        print("modal dialog")
}