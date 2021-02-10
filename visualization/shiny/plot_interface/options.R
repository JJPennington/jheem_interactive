

AGE.OPTIONS <- list(
    values = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"),
    names = c("13-24 years", "25-34 years", "35-44 years", "45-54 years", "55+ years"),
    code = 'age',
    label = "Age Group"
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
    label = "Race/Ethnicity"    
)

SEX.OPTIONS <- list(
    values = c("male","female"),
    names = c("Male", "Female"),
    code = 'sex',
    label = "Biological Sex"    
)

RISK.OPTIONS.1 <- list(
    values = c("msm", "idu", "msm_idu", "heterosexual"),
    names = c("MSM", "IDU", "MSM+IDU", "Heterosexual"),
    code = 'risk',
    label = "Risk Factor"    
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
    values = c('incidence','new','prevalence','mortality'),
    names = c("Incidence", "Reported Diagnoses", "Prevalence (Diagnosed)", "HIV Mortality"),
    code = 'outcome',
    label = "Outcome"
)

PLOT.FORMAT.OPTIONS <- list(
    values = c('mean.and.interval', 'individual.simulations'),
    names = c("The Mean Estimate and 95% Credible Interval", "Each Individual Simulation"),
    code = 'plot.format',
    label = "Plot Format"
)


##-----------------------------------------##
##-- OLDER - for backwards compatibility --##
##-----------------------------------------##

AGES = list(
    name='age-groups',
    shortName='age',
    label='Age',
    choices=c(
        age1='13-24 years',
        age2='25-34 years',
        age3='35-44 years',
        age4='45-54 years',
        age5='55+ years') )

RACES = list(
    name='racial-groups',
    shortName='race',
    label='Race',
    choices=c(
        black="Black",
        hispanic="Hispanic",
        other="Other") )

SEXES = list(
    name='sex',
    shortName='sex',
    label='Sex',
    choices=c(
        male='Male',
        female='Female') )

RISKS = list(
    name='risk-groups',
    shortName='risk',
    label='Risk Factor',
    choices=c(
        msm="MSM",
        idu="IDU",
        msm_idu="MSM+IDU",
        heterosexual="Heterosexual") )

RISKS2 = list(
    name='risk-groups',
    shortName='risk',
    label='Risk Factor',
    choices=c(
        msm="MSM",
        iduActive="Active IDU",
        iduPrior="Prior IDU",
        msm_iduActive="MSM + active IDU",
        msm_iduPrior="MSM + prior IDU",
        heterosexual="Heterosexual") )

DIMENSION.VALUES = list(
    age=AGES,
    race=RACES,
    sex=SEXES,
    risk=RISKS)

DIMENSION.VALUES2 = list(
    age=AGES,
    race=RACES,
    sex=SEXES,
    risk=RISKS2)

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
        DIMENSION.VALUES
    else
        DIMENSION.VALUES2
}
