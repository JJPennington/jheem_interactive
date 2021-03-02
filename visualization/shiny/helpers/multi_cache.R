
library(aws.iam)
library(aws.s3)

##-- CONSTRUCTOR --##

# A multi-cache is a list with elements
# $disk.caches - a list
# $mem.cache

create.multi.cache <- function(mem.cache,
                               disk.caches)
{
    list(mem.cache = mem.cache,
         disk.caches = disk.caches,
         explicit.caches = list()
    )
}


##-- GETTERS AND SETTERS --##

get.simsets.from.cache <- function(codes,
                                   cache)
{
    codes = gsub('\\.Rdata', '', codes)
    
    lapply(codes, function(code){
        key = codes.to.keys.for.cache(code)
        
        #see if it's in an explicit cache
        from.explicit = get.simset.from.explicit.cache(code, cache)
        if (!is.null(from.explicit))
            from.explicit
        #else if in the mem cache, return it
        else if (cache$mem.cache$exists(key))
            cache$mem.cache$get(key, missing=NULL)
        else
        {
            disk.cache = cache$disk.caches[[which.disk.cache.for.simset(code)]]
        #else if in the disk cache, put it in the mem cache and return it
            if (disk.cache$exists(key))
            {
                simset = disk.cache$get(key, missing=NULL)
                cache$mem.cache$set(key, simset)
                simset
            }
        #else return null
            else
                NULL
        }
    })
}

#I don't think I actually use this
put.simsets.to.cache <- function(codes,
                                 simsets,
                                 cache)
{
    codes = gsub('\\.Rdata', '', codes)
    keys = codes.to.keys.for.cache(codes)
    
    for (i in 1:length(codes))
    {
        # put it in the appropriate disk cache
        disk.cache = cache$disk.caches[[which.disk.cache.for.simset(codes[[i]])]]
        disk.cache$set(keys[i], simsets[[i]])
        
        # put it in the mem cache
        cache$mem.cache$set(keys[i], simsets[[i]])
    }
}

are.simsets.in.disk.cache <- function(codes, cache)
{
    codes = gsub('\\.Rdata', '', codes)
    
    sapply(codes, function(code){
        key = codes.to.keys.for.cache(code)
        cache$disk.caches[[which.disk.cache.for.simset(code)]]$exists(key)
    })
}

# Put in the appropriate disk cache
# AND put in mem cache
# Returns true if successful, false if there was an error
pull.simsets.to.cache <- function(session,
                                  codes,
                                  cache,
                                  pull.to.explicit = NULL)
{
    print(codes)
    codes = gsub('\\.Rdata', '', codes)
    
    for (code in codes)
    {
        key = codes.to.keys.for.cache(code)
        disk.cache = cache$disk.caches[[which.disk.cache.for.simset(code)]]
        if (disk.cache$exists(key))
        {
        # if in both mem and disk cache, done
            if (cache$mem.cache$exists(key))
            {}
        # else if in disk but not mem cache, put to mem
            else
                cache$mem.cache$set(key, disk.cache$get(key, missing=stop("Error in cache"), exec_missing=T))
        }
        # else if in mem but not disk cache, put to disk
        else if (cache$mem.cache$exists(key))
            disk.cache$set(key, cache$mem.cache$get(key, missing=stop("Error in cache"), exec_missing=T))
        # else pull from s3, put it in appropriate disk cache and in mem cache
        else
        {
            filename = paste0(code, '.Rdata')
#            print(paste0("pulling from s3: ", filename))
            
            tryCatch({
                
                if (is.sim.stored(filename))
                    simset = sims.load(filename)
                else    
                {
                    show.error.message(session,
                                       title="Simulation File(s) Unavailable",
                                       message="The simulation file(s) needed are not currently available on the remote server. We apologize, but we cannot process the requested interventions at this time.")
                    
                    if (!is.null(pull.to.explicit))
                        return (NULL)
                    else
                        return (F)
                }
            },
            error = function(e){
                show.error.message(session,
                                   title="Error Retrieving File(s)",
                                   message = "There was an unexpected error while retrieving the file(s) from the remote server. We apologize - please try again in a few minutes.")
                
                if (!is.null(pull.to.explicit))
                    return (NULL)
                else
                    return (F)
            })
            
            if (!is.null(pull.to.explicit))
            {
                cache = put.simset.to.explicit.cache(codes=code,
                                                     simsets = simset,
                                                     cache = cache,
                                                     explicit.name = pull.to.explicit)
            }
            else
            {
                disk.cache$set(key, simset)
                cache$mem.cache$set(key, simset)
            }
        }
    }
        
    #-- Return TRUE for success --#
    if (!is.null(pull.to.explicit))
        return (cache)
    else
        return (T)
}

##---------------------##
##-- EXPLICIT CACHES --##
##---------------------##

#returns the cache
put.simset.to.explicit.cache <- function(codes,
                                         simsets,
                                         cache,
                                         explicit.name)
{
    if (!is(simsets, 'list'))
        simsets = list(simsets)
    
    if (!any(names(cache$explicit.caches)==explicit.name))
        cache$explicit.caches[[explicit.name]] = list()
    
    keys = codes.to.keys.for.cache(codes)
    names(simsets) = keys
    cache$explicit.caches[[explicit.name]][keys] = simsets
    
    cache
}

#returns the cache
remove.simsets.from.explicit.cache <- function(codes,
                                               cache,
                                               explicit.name)
{
    keys = codes.to.keys.for.cache(codes)
    if (!is.null(cache$explicit.caches[[explicit.name]]))
        cache$explicit.caches[keys] = NULL
    cache
}

are.simsets.in.explicit.cache <- function(codes,
                                          cache,
                                          explicit.names=names(cache$explicit.caches))
{
    explicit.names = intersect(explicit.names, names(cache$explicit.caches))
    keys = codes.to.keys.for.cache(codes)
    sapply(keys, function(key){
        any(sapply(cache$explicit.caches[explicit.names], function(ex){
            any(names(ex)==key)
        }))
    })
}

get.simset.from.explicit.cache <- function(code,
                                           cache,
                                           explicit.names=names(cache$explicit.caches))
{
    explicit.names = intersect(explicit.names, names(cache$explicit.caches))
    key = codes.to.keys.for.cache(code)
    for (ex in cache$explicit.caches[explicit.names])
    {
        if (any(names(ex)==key))
            return (ex[[key]])
    }
    
    NULL
}

thin.explicit.cache <- function(keep.codes,
                                cache,
                                explicit.name)
{
    keys = codes.to.keys.for.cache(keep.codes)
    keys = intersect(keys, names(cache$explicit.caches[[explicit.name]]))
    
    cache$explicit.caches[[explicit.name]] = cache$explicit.caches[[explicit.name]][keys]
    
    cache
}

##-------------##
##-- HELPERS --##
##-------------##

which.disk.cache.for.simset <- function(codes)
{
    codes = gsub("\\.Rdata$", '', codes)
    regex = "^[^_]+_[^_]+_(.+)$"
    intervention.subcodes = gsub(regex, "\\1", codes)
    
    rv = rep(2, length(codes))
    rv[intervention.subcodes=='noint' | intervention.subcodes=='baseline']
    rv
}

codes.to.keys.for.cache <- function(codes)
{
    keys = gsub('\\.Rdata', '', codes)
    keys = tolower(keys)
    keys = str_replace_all(keys, "[[:punct:]]", "")
    keys
}