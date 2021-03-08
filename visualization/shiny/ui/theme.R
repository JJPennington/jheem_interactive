
library(bslib)


create.theme <- function()
{
    bs_theme(
        bootswatch = 'flatly',# 'flatly',
        primary = '#1c4882',#'#002d72',
        info = '#dceef2',#'#87afe5',
        'navbar-light-hover-color' = 'red',
        'navbar-dark-hover-color' = 'red'
    )
    
}