

get.ip.address.from.req <- function(req)
{
    if (!is.null(req[["HTTP_X_FORWARDED_FOR"]]))
        req[["HTTP_X_FORWARDED_FOR"]]
    else
        req[["REMOTE_ADDR"]]
}

get.geolocation <- function(req)
{
    tryCatch({
        #ip = geoloc::wtfismyip()$Your_IPAddress
        ip = get.ip.address.from.req(req)
        print(paste0("IP = ", ip))
        

        if (is.null(ip))
            return (NULL)
        ip.info = ip_api(ip)
        if (is.null(ip.info) || is.na(ip.info['latitude']) || is.na(ip.info['longitude']))
            return (NULL)
        
        list(
            latitude = as.numeric(ip.info['latitude']),
            longitude = as.numeric(ip.info['longitude'])
        )
    },
    error = function(e){
        print("There was an error getting the geolocation - skipping")
        print(e$message)
        NULL
    })
    
}

get.closest.msa <- function(req,
                            msas=unique(get.prerun.intervention.codes()$location),
                            loc=get.geolocation(req))
{
    if (is.null(loc))
        return (NULL)
    
    lat.long = c(loc$longitude, loc$latitude)
    msa.loc = get.location.latitude.longitude(msas)
    msa.lat.long = cbind(msa.loc$longitude, msa.loc$latitude)
    
    dists = geosphere::distGeo(lat.long, msa.lat.long)
    
    msas[order(dists)][1]
}