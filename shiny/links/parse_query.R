
QUERY.DELIMETER = ','

parse.query.settings <- function(query.string)
{
    # pull a version for development if we're on a local machine
    is.local = Sys.getenv('SHINY_PORT') == ''
  
    if (is.local)
        default.web.version.data = get.web.version.data(WEB.VERSION.IN.DEVELOPMENT)
    else
        default.web.version.data = ALL.WEB.VERSION.DATA[[1]]
    
    rv = list(
        location=NULL,
        web.version.data=NULL,
        link.data=NULL,
        link.query=NULL
    )
    
    
    if (query.string == '')
    {}
    else
    {
        query.string.elements = strsplit(substr(query.string, 2, nchar(query.string)),
                                                QUERY.DELIMETER)[[1]]
        
        #-- parse locations --#
        locations.from.query = lapply(query.string.elements, match.location.name)
        locations.from.query.are.null = sapply(locations.from.query, is.null)
        if (any(!locations.from.query.are.null))
        {
            location = locations.from.query[!locations.from.query.are.null][[1]]
            version.compatible.with.location = sapply(ALL.WEB.VERSION.DATA, function(web.version.data){
                any(web.version.data$locations==location)
            })
            if (any(version.compatible.with.location))
                rv$location = location
        }
            
        #-- parse web version --#
        web.version.data.from.query = lapply(query.string.elements, function(qse){
            mask = tolower(qse) == WEB.VERSION.QUERY.STRINGS
            if (any(mask))
                ALL.WEB.VERSION.DATA[mask][[1]]
            else
                NULL
        })
        web.versions.from.query.are.null = sapply(web.version.data.from.query, is.null)
        if (any(!web.versions.from.query.are.null))
        {
            web.version.data = web.version.data.from.query[!web.versions.from.query.are.null][[1]]
            if (is.null(rv$location) || any(web.version.data$locations==rv$location))
                rv$web.version.data = web.version.data
        }
        
        #-- parse links --#
        if (all(locations.from.query.are.null) && all(web.versions.from.query.are.null))
        {
            link.data.exists = sapply(query.string.elements, link.exists)
            
            if (any(link.data.exists))
            {
                link.data.from.query = get.link.data(query.string.elements[link.data.exists][1])
                if (!is.null(link.data.from.query))
                {
                    rv$link.data = link.data.from.query
                    rv$link.query = query.string.elements[link.data.exists][1]
                    
                    rv$location = rv$link.data$main.settings$location
                    rv$web.version.data = get.web.version.data(rv$link.data$web.version)
                }
            }
        }
            
    }
    
    if (is.null(rv$web.version.data) && is.null(rv$location))
    {
        rv$web.version.data = default.web.version.data
    }
    else if (is.null(rv$web.version.data))
    {
        version.compatible.with.location = sapply(ALL.WEB.VERSION.DATA, function(web.version.data){
            any(web.version.data$locations==location)
        })
        
        rv$web.version.data = ALL.WEB.VERSION.DATA[version.compatible.with.location][[1]]
    }
    
    rv
}