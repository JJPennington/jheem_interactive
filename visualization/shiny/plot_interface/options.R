

AGE.OPTIONS <- list(
    values = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"),
    names = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"),
    code = 'age',
    label = "Age Group",
    label.plural = 'Age Groups'
)


#example for how to use
if (1==2)
{
    checkboxGroupInput(inputId='yourid',
                       label = AGE.OPTIONS$label,
                       choiceValues = AGE.OPTIONS$values,
                       choiceNames = AGE.OPTIONS$names,
                       selected = AGE.OPTIONS$values
                       )
}

RACE.OPTIONS <- list(
    values = c("black", "hispanic","other"),
    names = c("Black", "Hispanic", "Other"),
    code = 'race',
    label = "Race/Ethnicity",
    label.plural = 'Races'
)

SEX.OPTIONS <- list(
    values = c("male","female"),
    names = c("Male", "Female"),
    code = 'sex',
    label = "Biological Sex",
    label.plural = 'Sexes'
)

RISK.OPTIONS.1 <- list(
    values = c("msm", "idu", "msm_idu", "heterosexual"),
    names = c("MSM", "IDU", "MSM+IDU", "Heterosexual"),
    code = 'risk',
    label = "Risk Factor",
    label.plural = 'Risk Factors'
)

RISK.OPTIONS.2 <- list(
    values = c("msm", "active_idu", "prior_idu", "msm_active_idu", "msm_prior_idu", "heterosexual"),
    names = c("MSM", "Active IDU", "Prior IDU", "MSM + Active IDU", "MSM + Prior IDU", "Heterosexual"),
    code = 'risk',
    label = "Risk Factor",
    label.plural = 'Risk Factors'
)


ALL.DIMENSIONS = list(AGE.OPTIONS,
                   RACE.OPTIONS,
                   SEX.OPTIONS,
                   RISK.OPTIONS.1)

ALL.DIMENSION.VALUES = lapply(ALL.DIMENSIONS, function(o){o$values})
names(ALL.DIMENSION.VALUES) = lapply(ALL.DIMENSIONS, function(o){o$code})


DIMENSION.OPTIONS.1 = list(
    values = sapply(ALL.DIMENSIONS, function(d){d$code}),
    names = sapply(ALL.DIMENSIONS, function(d){d$label})
)


YEAR.OPTIONS <- list(
    values = 2020:2030,
    names = as.character(2020:2030),
    code = 'year',
    label = 'Year'
)

OUTCOME.OPTIONS <- list(
    values = c('incidence','new','prevalence','mortality','testing.period','prep','suppression'),
    names = c("Incidence", "Reported Diagnoses", "Prevalence (Diagnosed)", "HIV Mortality","HIV Testing","PrEP Uptake","Viral Suppression"),
    code = 'outcome',
    label = "Outcome"
)

PLOT.FORMAT.OPTIONS <- list(
    values = c('mean.and.interval', 'individual.simulations'),
    names = c("The Mean Estimate and 95% Credible Interval", "Each Individual Simulation"),
    code = 'plot.format',
    label = "Plot Format"
)


TESTING.OPTIONS <- list(
    values = c(3,6,12,24),
    code = 'testing',
    label = "Testing"
)
TESTING.OPTIONS$names = paste0(TESTING.OPTIONS$values, " months")


PREP.OPTIONS <- list(
    values = c(0.10, 0.25, 0.5, 0.75),
    code = 'prep',
    label = "PrEP"
)
PREP.OPTIONS$names = paste0(100*PREP.OPTIONS$values, "%")


SUPPRESSION.OPTIONS <- list(
    values = c(0.80, 0.85, 0.90, 0.95),
    code = 'suppression',
    label = "Suppression"
)
SUPPRESSION.OPTIONS$names = paste0(100*SUPPRESSION.OPTIONS$values, "%")


make.named.choices <- function(choiceValues, choiceNames)
{
    names(choiceValues) = choiceNames
    choiceValues
}

##-- AGGREGATE AGE/SEX/RACE/RISK --##

DIMENSION.VALUES.1 = list(
    age=AGE.OPTIONS,
    race=RACE.OPTIONS,
    sex=SEX.OPTIONS,
    risk=RISK.OPTIONS.1)

DIMENSION.VALUES.2 = list(
    age=AGE.OPTIONS,
    race=RACE.OPTIONS,
    sex=SEX.OPTIONS,
    risk=RISK.OPTIONS.2)

#'@description Get the potential values (which can be subsetted) for each
#' dimension
#'@param version The indicator for the version of the model. Corresponds
#' to one of the values of names(get.version.options)
#'@param location A location code. Corresponds to one of the values of
#' names(get.location.options(version))
#'@return A named list of character vectors. 
#' The names of the list correspond to names(get.facet.by.options()) or
#'  names(get.split.by.options())
#' Each element in the list is a named character vector of 
#' possible values. 
#'  names(get.dimension.value.options()[[d]]) correspond to the values of
#'   get.facet.by.options() or get.split.by.options(), and should be
#'    passed to plot.simulations() function via the dimension.subsets
#'     argument
#'  the values of get.dimension.value.options()[[d]] are 'displayable'
#'   value names
get.dimension.value.options <- function(
    version, location, msm_idu_mode=FALSE)
{
    if (msm_idu_mode == FALSE)
        DIMENSION.VALUES.1
    else
        DIMENSION.VALUES.2
}
