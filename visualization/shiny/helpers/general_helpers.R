

##-- LUMPING IDU --##

lump.idu.for.intervention <- function(int)
{
    tpops = get.target.populations.for.intervention(int)
    mapped = lapply(tpops, function(tpop){
        lumped = lump.idu.in.target.population(tpop)
        if (target.populations.equal(tpop, lumped))
            tpop
        else
        {
            lumped.already.present = any(sapply(int$raw, function(sub){
                any(sapply(sub$target.populations, target.populations.equal, tpop))
            }))
            
            if (lumped.already.present)
                lumped
            else
                tpop
        }
    })
    names(mapped) = sapply(tpops, target.population.hash)
    
    for (i in 1:length(int$raw))
    {
        for (j in 1:length(int$raw[[i]]$target.populations))
            int$raw[[i]]$target.populations[[j]] = 
                mapped[[target.population.hash(int$raw[[i]]$target.populations[[j]]) ]]
    }
    
    int
}

lump.idu.in.name <- function(name)
{
    gsub('active and prior IDU', 'IDU', name, ignore.case = T)
}


lump.idu.in.target.population <- function(tpop)
{
    lumped = tpop
    lumped[,,,'IDU_in_remission'] = lumped[,,,'active_IDU']
    lumped
}