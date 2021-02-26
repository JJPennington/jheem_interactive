

# The database needs to track all these, plus the date/time
track.request <- function(session.id, #non-unique string
                          called.from, #string
                          location, #string
                          intervention.code, #string
                          n.target.subgroups, #integer
                          intervention.start, #numeric
                          intervention.end, #numeric
                          target.ages, #string
                          target.races, #string
                          target.sexes, #string
                          target.risks, #string
                          testing, #may be NA
                          prep, #may be NA
                          viral.suppression, #may be NA
                          outcomes, #string
                          facet.by, #string
                          split.by, #string
                          change.outcome.start, #numeric
                          change.outcome.end, #numeric
                          show.change, #numeric
                          plot.format, #string
                          show.ages, #string
                          show.races, #string
                          show.sexes, #string
                          show.risks #string
                          )
{
    
}