#-- Interventions --#

#orig.wd = getwd()
#setwd('../Ending_HIV/')
#source('code/source_code.R')
#setwd(orig.wd)

import.cache.simsets <- function(src.dir='../Ending_HIV/mcmc_runs/visualization_simsets',
                                 dst.dir='shiny/sim_cache')
{
    filenames = list.files(src.dir, 
                           recursive = T,
                           include.dirs = F)
    
    mask = grepl('baseline', filenames) | grepl('noint', filenames)
    filenames = filenames[mask]
    
    filenames.minus.path = gsub("^.*/([^/]*$)", "\\1", filenames)
    
    file.copy(from = file.path(src.dir, filenames),
              to = file.path(dst.dir, filenames.minus.path),
              overwrite = T)
}

setup.misc.cache <- function(interventions,
                             dst.dir='shiny/cache')
{
    interventions = interventions[!sapply(interventions, is.null.intervention)]
    CACHED.SIMS.NAMES = sapply(interventions, get.intervention.code) 
    print(CACHED.SIMS.NAMES)
    save(CACHED.SIMS.NAMES, file=file.path(dst.dir, 'CACHED.SIMS.NAMES.Rdata'))
}

setup.misc.cache.from.dir <- function(src.dir='../Ending_HIV/mcmc_runs/visualization_simsets',
                             dst.dir='shiny/cache')
{
    # Set up the list of all intervention file names
    
    filenames = list.files(src.dir, 
                           recursive = T,
                           include.dirs = F)
    
    CACHED.SIMS.NAMES = gsub("^.*/([^/]*$)", "\\1", filenames)
    
    
    save(CACHED.SIMS.NAMES, file=file.path(dst.dir, 'CACHED.SIMS.NAMES.Rdata'))
}

print("SETTING UP FILENAMES:")
setup.misc.cache.from.dir()
print("DONE")

print("")

print("COPYING SIM FILES:")
import.cache.simsets()
print("DONE")