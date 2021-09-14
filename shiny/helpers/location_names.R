get.location.long.name <- function(location)
{
    unlist(msa.names(location))
}


NYC.MSA = '35620'
MIAMI.MSA = '33100'
LA.MSA = '31080'

ATLANTA.MSA = '12060'
HOUSTON.MSA = '26420'
DALLAS.MSA = '19100'

CHICAGO.MSA = '16980'
DC.MSA = '47900'
PHILADELPHIA.MSA = '37980'

ORLANDO.MSA = '36740'
SF.MSA = '41860'
PHOENIX.MSA = '38060'

TAMPA.MSA = '45300'
RIVERSIDE.MSA = '40140'
DETROIT.MSA = '19820'

BALTIMORE.MSA = '12580'
VEGAS.MSA = '29820'
BOSTON.MSA = '14460'

SAN.DIEGO.MSA = '41740'
CHARLOTTE.MSA = '16740'
SAN.ANTONIO.MSA = '41700'

JACKSONVILLE.MSA = '27260'
NEW.ORLEANS.MSA = '35380'
MEMPHIS.MSA = '32820'

SEATTLE.MSA = '42660'
AUSTIN.MSA = '12420'
INDIANAPOLIS.MSA = '26900'

CINCINATTI.MSA = '17140'
COLUMBUS.MSA = '18140'
BATON.ROUGE.MSA = '12940'

SACRAMENTO.MSA = '40900'
CLEVELAND.MSA = '17460'

MSA.SHORT.NAMES = list(
    '35620' = 'NYC',
    '33100' = 'Miami',
    '31080' = 'LA',
    '12060' = 'Atlanta',
    '26420' = 'Houston',
    '19100' = 'Dallas',
    '16980' = 'Chicago',
    '47900' = c("Washington D.C.", 'DC'),
    '37980' = 'Philadelphia',
    '36740' = 'Orlando',
    '41860' = 'SF',
    '38060' = 'Phoenix',
    '45300' = 'Tampa',
    '40140' = 'Riverside',
    '19820' = 'Detroit',
    '12580' = 'Baltimore',
    '29820' = 'Vegas',
    '14460' = 'Boston',
    '41740' = 'San_Diego',
    '16740' = 'Charlotte',
    '41700' = 'San Antonio',
    '27260' = 'Jacksonville',
    '35380' = 'New Orleans',
    '32820' = 'Memphis',
    '42660' = 'Seattle',
    '12420' = 'Austin',
    '26900' = 'Indianapolis',
    '17140' = 'Cincinatti',
    '18140' = 'Columbus',
    '12940' = 'Baton_Rouge',
    '40900' = 'Sacramento',
    '17460' = 'Cleveland',
    '41180' = c("St. Louis", "Saint Louis")
)

MSA.SHORT.NAMES.UNDERSCORED = lapply(MSA.SHORT.NAMES, function(msa.names){
    tolower(gsub(" ", "_", gsub("\\.", '', msa.names)))
})

MSA.SHORT.NAMES.CONDENSED = lapply(MSA.SHORT.NAMES.UNDERSCORED, function(msa.names){
    tolower(gsub("_", "", msa.names))
})

get.location.short.name <- function(location)
{
    MSA.SHORT.NAMES[[location]][1]
}

match.location.name <- function(name)
{
    name = tolower(name)
    mask = sapply(names(MSA.SHORT.NAMES), function(msa){
        any(tolower(MSA.SHORT.NAMES[[msa]])==name) ||
            any(MSA.SHORT.NAMES.UNDERSCORED[[msa]]==name) ||
            any(MSA.SHORT.NAMES.CONDENSED[[msa]]==name)
    })
    
    if (any(mask))
        names(MSA.SHORT.NAMES)[mask][1]
    else
        NULL
}