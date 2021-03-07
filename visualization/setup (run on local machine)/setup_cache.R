

import.cache.simsets <- function(src.dir='../Ending_HIV/mcmc_runs/visualization_simsets',
                                 dst.dir='visualization/shiny/sim_cache')
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

setup.misc.cache <- function(src.dir='../Ending_HIV/mcmc_runs/visualization_simsets',
                             dst.dir='visualization/shiny/cache')
{
    # Set up the list of all intervention file names
    
    filenames = list.files(src.dir, 
                           recursive = T,
                           include.dirs = F)
    
    CACHED.SIMS.NAMES = gsub("^.*/([^/]*$)", "\\1", filenames)
    
    
    save(CACHED.SIMS.NAMES, file=file.path(dst.dir, 'CACHED.SIMS.NAMES.Rdata'))
}

print("SETTING UP FILENAMES:")
setup.misc.cache()
print("DONE")

print("")

print("COPYING SIM FILES:")
import.cache.simsets()
print("DONE")