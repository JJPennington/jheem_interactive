
# To specify a new version
#   Create a web.version.data object for it in the list ALL.WEB.VERSIONS
#   Create the function implementations corresponding to any function names given as arguments to make.web.version.data

##------------------##
##-- VERSION DATA --##
##------------------##

DEFAULT.MODEL.VERSION = '1.0'

read.locations.for.version <- function(version.name)
{
    load(file.path('cache/locations_for_version', paste0(version.name, '.Rdata')))
    locations
}

make.web.version.data <- function(name, 
                                  query, #the element of the URL query string that points to this
                                  app.title = "JHEEM: Ending HIV in the US", #The text title for a web page
                                  
                                  #Location meta-data
                                  locations = read.locations.for.version(name),
                                  location.resolution = 'City',
                                  location.popover = paste0("Choose from among ", length(locations), " Metropolitan Statistical Areas. These encompass the 48 high-burden counties plus Washington DC identified by the Ending the HIV Epidemic Initiative."),
                                  
                                  #Naming of simulations
                                  noint.name = 'No Intervention',
                                  intervention.name = 'Intervention',
                                  
                                  #Other
                                  min.pre.intervention.year=2010,
                                  min.intervention.year=2020,
                                  max.intervention.year=2030,
                                  
                                  #Model Versions
                                  baseline.model.version=DEFAULT.MODEL.VERSION,
                                  noint.model.version=DEFAULT.MODEL.VERSION,
                                  interventions.model.version=DEFAULT.MODEL.VERSION,
                                  seed.model.version=DEFAULT.MODEL.VERSION,
                                  
                                  #Labels/UI stuff
                                  intervention.label='Intervention',
                                  intervention.label.article='an',
                                  overview.page='overview.html',
                                  default.plot.statistic = 'time.diff.relative',
                                  
                                  #Error Checking
                                  check.prerun.errors=function(...){NULL}, #takes one argument - settings
                                  check.custom.unit.errors=error.check.custom.ehe.intervention.unit, 
                                    #^takes two arguments - settings, i - returns either NULL (no errors) or a character vector of one or more error messages
                                  
                                  #UI creation functions
                                  create.intervention.selector.panel.function=function(){create.intervention.selector.panel('prerun')},
                                  create.custom.intervention.unit.selector.function = create.custom.intervention.unit.selector,
                                    #^ takes two arguments: i, web.version.data
                                  setup.outputs = function(...){}, #by default, do nothing
                                  allow.intervention.summary = T,
                                  make.intervention.summary = make.ehe.intervention.pretty.table,
                                      #^ takes two inputs: int, suffix
                                  
                                  #-- UI getters and setters --#
                                  get.prerun.settings.function=get.ehe.prerun.settings, #takes one argument - input
                                  set.prerun.settings.function=set.ehe.prerun.settings, #takes three arguments - session, input, settings
                                  get.prerun.intervention.codes.from.settings = function(settings){settings$intervention.codes}, #takes one argument - settings
                                  get.prerun.filter.from.settings = function(settings){settings$filter}, #takes one argument - settings
                                  apply.prerun.filter.function = filter.simset.by.parameters, #takes two arguments - simset, filter
                                  
                                  get.custom.unit.settings.function=get.ehe.custom.unit.settings, #takes two arguments - input, n.subpopulations
                                  set.custom.unit.settings.function=set.ehe.custom.units.to.settings, #takes three arguments - session, input, settings
                                  get.custom.intervention.from.settings=get.ehe.custom.intervention.from.settings, #takes one argument - settings
                                  get.custom.filter.from.settings = function(settings){settings$filter},
                                  apply.custom.filter.function = filter.simset.by.parameters, #takes two arguments - simset, filter
                                  
                                  #-- Tracking for Analytics --#
                                  prerun.settings.to.trackable.function=function(settings){list()}, #by default, nothing
                                    #^ takes one argument - settings
                                  intervention.to.trackable.function=ehe.intervention.to.trackable,
                                    #^ takes one argument - intervention

                                  #-- For running simulations --#
                                  simulate.function = run.simset.intervention,
                                      # ^ takes arguments: simset, intervention, run.from.year, run.to.year, keep.years, update.progress
                                  run.simulations.to.year = max.intervention.year,
                                  interventions.equal=web.tool.interventions.equal #takes two arguments - int1 and int2
                                  )
{
    list(
        name=name,
        query=query,
        app.title = app.title,
        
        locations=locations,
        location.resolution = location.resolution,
        location.popover = location.popover,
        
        noint.name = noint.name,
        intervention.name = intervention.name,
        
        min.pre.intervention.year = min.pre.intervention.year,
        min.intervention.year = min.intervention.year,
        max.intervention.year = max.intervention.year,
        
        baseline.model.version=baseline.model.version,
        noint.model.version=noint.model.version,
        interventions.model.version=interventions.model.version,
        seed.model.version=seed.model.version,
        
        intervention.label=intervention.label,
        intervention.label.article=intervention.label.article,
        overview.page=overview.page,
        default.plot.statistic=default.plot.statistic,
        
        create.intervention.selector.panel.function=create.intervention.selector.panel.function,
        create.custom.intervention.unit.selector.function=create.custom.intervention.unit.selector.function,
        setup.outputs=setup.outputs,
        
        allow.intervention.summary = allow.intervention.summary,
        make.intervention.summary = make.intervention.summary,
        
        check.prerun.errors=check.prerun.errors,
        check.custom.unit.errors=check.custom.unit.errors,
        
        get.prerun.settings.function=get.prerun.settings.function,
        set.prerun.settings.function=set.prerun.settings.function,
        get.prerun.intervention.codes.from.settings=get.prerun.intervention.codes.from.settings,
        get.prerun.filter.from.settings=get.prerun.filter.from.settings,
        apply.prerun.filter.function=apply.prerun.filter.function,
        
        get.custom.unit.settings.function=get.custom.unit.settings.function,
        set.custom.unit.settings.function=set.custom.unit.settings.function,
        get.custom.intervention.from.settings=get.custom.intervention.from.settings,
        get.custom.filter.from.settings=get.custom.filter.from.settings,
        apply.custom.filter.function=apply.custom.filter.function,
        
        prerun.settings.to.trackable.function=prerun.settings.to.trackable.function,
        intervention.to.trackable.function=intervention.to.trackable.function,
        
        simulate.function = simulate.function,
        run.simulations.to.year = run.simulations.to.year,
        interventions.equal=interventions.equal
    )
}

ALL.WEB.VERSION.DATA = list(
    
    #-- The DEFAULT - EHE version 1.0 --#
    make.web.version.data(name='EHE_1_0', query='ehe'),
    
    #-- COVID --#
    make.web.version.data(name='covid_1_0', query='covid',
                          overview.page='covid_overview.html',
                          app.title = 'JHEEM: The Effects of COVID-19 on HIV Transmission',
                          
                          noint.name = 'Absent COVID',
                          intervention.name = 'COVID Scenario',
                          
                          min.pre.intervention.year = 2015,
                          max.intervention.year = 2025,
                          default.plot.statistic='cumulative.diff.relative',
                          
                          interventions.model.version = 'covid.4.0',
                          intervention.label='Scenario', 
                          intervention.label.article='a',
                          
                          check.prerun.errors = check.covid.prerun.errors,
                          check.custom.unit.errors = check.covid.custom.unit.errors,
                          
                          setup.outputs=setup.covid.sim.counter,
                          create.intervention.selector.panel.function = create.covid.prerun.scenario.selector.panel,
                          create.custom.intervention.unit.selector.function = create.covid.custom.intervention.unit.selector,
                          
                          allow.intervention.summary = F,
                          
                          get.prerun.settings.function=get.covid.prerun.settings, #takes one argument - input
                          set.prerun.settings.function=set.covid.prerun.to.settings, #takes three arguments - session, input, settings
                          
                          get.custom.unit.settings.function = get.custom.covid.unit.settings,
                          set.custom.unit.settings.function = set.custom.covid.unit.settings,
                          get.custom.intervention.from.settings = get.covid.intervention.from.settings,
                          
                          prerun.settings.to.trackable.function = covid.prerun.settings.to.trackable,
                          intervention.to.trackable.function = covid.intervention.to.trackable,
                          
                          simulate.function = run.covid.simset.intervention,
                          interventions.equal=covid.interventions.equal
    )
)
ALL.WEB.VERSIONS = names(ALL.WEB.VERSION.DATA) = sapply(ALL.WEB.VERSION.DATA, function(v){v$name})

WEB.VERSION.QUERY.STRINGS = tolower(sapply(ALL.WEB.VERSION.DATA, function(wv){wv$query}))


WEB.VERSION.IN.DEVELOPMENT = 'EHE_1_0' #used to go straight to this web version for testing/development

##-------------------------##
##-- GETTERS and HELPERS --##
##-------------------------##

get.web.version <- function(input)
{
    input$web_version
}

get.web.version.data <- function(web.version)
{
    ALL.WEB.VERSION.DATA[[web.version]]
}

make.web.version.field <- function(web.version.data)
{
    div(style = "display: none;",
        textInput(inputId = "web_version",
                  label = 'web version',
                  value = web.version.data$name)
    )
}

