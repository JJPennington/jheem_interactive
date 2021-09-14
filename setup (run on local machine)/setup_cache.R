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

setup.covid.cache <- function(src.dir = '../Ending_HIV/mcmc_runs/covid_visualization_simsets',
                              dst.dir='shiny/cache')
{
    subdir = list.dirs(src.dir, recursive = F)[1]
    file = list.files(subdir)[1]
    
    load(file.path(subdir, file))
    COVID.PARAMETER.VALUES = simset@parameters[,c('sexual.transmission.reduction',
                                            'testing.reduction',
                                            'prep.reduction',
                                            'suppression.reduction',
                                            'sexual.transmission.increase')]
    
    save(COVID.PARAMETER.VALUES, file=file.path(dst.dir, 'COVID.PARAMETER.VALUES.Rdata'))
}

setup.cached.locations <- function(ehe.dir='../Ending_HIV/mcmc_runs/visualization_simsets/',
                                   covid.dir='../Ending_HIV/mcmc_runs/covid_visualization_simsets/',
                                   dst.dir='shiny/cache/locations_for_version')
{
    dirs = list(EHE_1_0=ehe.dir,
                covid_1_0=covid.dir)
    
    for (version.name in names(dirs))
    {
        dir = dirs[[version.name]]
        filenames = list.files(dir,
                               recursive = T,
                               include.dirs = F)
        
        locations = unique(sapply(filenames, function(ff){
            parse.simset.filenames(ff)['location']
        }))
        
        save(locations, file=file.path(dst.dir, paste0(version.name, '.Rdata')))
    }
}

print("SETTING UP LOCATIONS FOR VERSION")
setup.cached.locations()
print("DONE")

print("SETTING UP FILENAMES:")
setup.misc.cache.from.dir()
print("DONE")

print("")

print("COPYING SIM FILES:")
import.cache.simsets()
print("DONE")

print("")

print("SETTING UP COVID CACHE:")
setup.covid.cache()
print("DONE")