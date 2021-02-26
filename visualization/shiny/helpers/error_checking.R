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
  valid = TRUE
  subpop_errs = list()
  
  for (i in 1:get.custom.n.subpopulations(input)) {
    subpop_errs[[as.character(i)]] = list(
      'no_custom_options_selected'=FALSE,
      'msm_selections_without_males'=FALSE,
      # No demographics selected
      'no_demographics_selected'=list(
        'ages'=length(get.custom.ages(input, i)) < 1,
        'races'=length(get.custom.races(input, i)) < 1,
        'sexes'=length(get.custom.sexes(input, i)) < 1,
        'risks'=length(get.custom.risks(input, i)) < 1
    ))
    
    # MSM & !male
    risks = get.custom.risks(input, i)
    sexes = get.custom.sexes(input, i)
    msm_selected = (('msm' %in% risks) || ('msm_active_idu' %in% risks) || 
        ('msm_prior_idu' %in% risks))
    subpop_errs[[i]][['msm_selections_without_males']] = 
      !('male' %in% sexes) && msm_selected
    
    # No intervention details selected
    testing = get.custom.use.testing(input, i)
    prep = get.custom.use.prep(input, i)
    suppression = get.custom.use.suppression(input, i)
    subpop_errs[[i]][['no_custom_options_selected']] = 
      !testing && !prep && !suppression
  }
  
  # Create error message
  message_list = list()
  for (i in 1:length(names(subpop_errs))) {
    # Find issues
    # - to-do: refactor?
    report = subpop_errs[[i]]
    demog_report = report[['no_demographics_selected']]
    demog_issue_groups = c()
    for (group in names(demog_report))
      if (demog_report[[group]])
        demog_issue_groups = c(demog_issue_groups, group)
    
    subpop_issues_found = report[['no_custom_options_selected']] ||
      report[['msm_selections_without_males']] || 
      length(demog_issue_groups) > 0
    
    # Assemble subpop message
    if (!subpop_issues_found)
      break
    
    header_i = paste0('<h4>Subpopulation ', as.character(i), '</h4>')
    msm_err_i = ''
    custom_err_i = ''
    demog_err_i = ''
    
    if (report[['msm_selections_without_males']])
      msm_err_i = paste0(
        '<p><strong>MSM: </strong>', 'Sex "male" must be selected if any MSM 
        options are selected under "risk factors".</p>')
    if (report[['no_custom_options_selected']])
      custom_err_i = paste0(
        '<p><strong>Intervention details: </strong>', 'At least 1 intervention
        detail (testing, PreP, or suppression) must be selected.</p>')
    if (length(demog_issue_groups) > 0)
      demog_err_i = paste0(
        '<p><strong>Demographics: </strong>', 'Please select 1 or more option 
        for the following categories: ', 
        paste(demog_issue_groups, collapse=', '),'</p>')
    
    message_list[[as.character(i)]] = paste0(
      header_i, msm_err_i, custom_err_i, demog_err_i, '<br/>')
  }  # </for>
  
  message = ''
  for (i in names(message_list))
    message = paste0(message, message_list[[as.character(i)]])
  
  # Handle validity
  valid = length(names(message_list)) < 1
  if (!valid)
    showMessageModal(title='Invalid selection(s)', message=HTML(message))

  return(valid)
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
      showMessageModal(paste0(
          INVALID_ERR_MSG_HEADER, 'At least one outcome must be checked.'))
    
    return(valid)
 }
