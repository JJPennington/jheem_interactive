# Functions ####
# This should have innate error handling and fail silently
# The database needs to track all these, plus the date/time
do.track.request <- function(session.id, #non-unique string
                             called.from, #string
                             version, #string
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
    tryCatch({
        df = data.frame(
            created_at=Sys.time(),
            session_id=session.id,
            called_from=called.from,
            version=version,
            location=location,
            intervention_code=intervention.code,
            n_target_subgroups=n.target.subgroups,
            intervention_start=intervention.start,
            intervention_end=intervention.end,
            target_ages=target.ages,
            target_races=target.races,
            target_sexes=target.sexes,
            target_risks=target.risks,
            testing=testing,
            prep=prep,
            viral_suppression=viral.suppression,
            outcomes=outcomes,
            facet_by=facet.by,
            split_by=split.by,
            change_outcome_start=change.outcome.start,
            change_outcome_end=change.outcome.end,
            show_change=show.change,
            plot_format=plot.format,
            show_ages=show.ages,
            show_races=show.races,
            show_sexes=show.sexes,
            show_risks=show.risks)
        db.write.rows(
            table.name='analytics',
            data.df=df)
    },
    error=function(e){
        log.error(e)
    })
}

# Testing ####
do.track.request(
    session.id='xxx', #non-unique string
    called.from='xxx', #string
    version='xxx', #string
    location='xxx', #string
    intervention.code='xxx', #string
    n.target.subgroups=3, #integer
    intervention.start=1.0, #numeric
    intervention.end=1.0, #numeric
    target.ages='xxx', #string
    target.races='xxx', #string
    target.sexes='xxx', #string
    target.risks='xxx', #string
    testing='', #may be NA
    prep=NA, #may be NA
    viral.suppression=NA, #may be NA
    outcomes='xxx', #string
    facet.by='xxx', #string
    split.by='xxx', #string
    change.outcome.start=1.5, #numeric
    change.outcome.end=999.8492, #numeric
    show.change=1.0, #numeric
    plot.format='xxx', #string
    show.ages='xxx', #string
    show.races='xxx', #string
    show.sexes='xxx', #string
    show.risks='xxx' #string
)
