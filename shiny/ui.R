
##-----------------##
##-- SOURCE CODE --##
##-----------------##

source('source_code.R')

#-- MAIN TAB CONTENT FILES --#
source('ui/contact.R', local=T)
source('ui/prerun_interventions.R', local=T)
source('ui/custom_interventions.R', local=T)


##-----------------------------------------------------##
##-- PRE-RENDER SOME EXPENSIVE-TO-RENDER UI ELEMENTS --##
##-----------------------------------------------------##

INTERVENTION.SELECTOR.PANELS = lapply(ALL.WEB.VERSION.DATA, function(wv){
    wv$create.intervention.selector.panel.function()
})
names(INTERVENTION.SELECTOR.PANELS) = ALL.WEB.VERSIONS

##------------------##
##-- DEFINE the UI--##
##------------------##

ui = function(req){

    #----------------------------#
    #-- PARSE THE QUERY STRING --#
    #----------------------------#
    
    # Parse the query string
    query.string = req$QUERY_STRING
    query.settings = parse.query.settings(query.string)
    
    # Set the selected tab
    selected.tab = NULL
    if (!is.null(query.settings$link.data))
    {
        if (query.settings$link.data$type=='prerun')
            selected.tab = 'prerun_interventions'
        else if (query.settings$link.data$type=='custom')
            selected.tab = 'custom_interventions'
    }
    else if (!is.null(query.settings$location))
        selected.tab = 'prerun_interventions'
    
    # If we didn't set a location in the query,
    #  try to pull closest from geolocation
    if (is.null(query.settings$location))
        query.settings$location = get.closest.msa(req)
    
    
    app.title = query.settings$web.version.data$app.title
    
    #----------------------------#
    #-- RENDER THE MAIN UI TAB --#
    #----------------------------#
          
    tags$html(style='height:100%',
              
              useShinydashboard(), #this call let's us use dashboard elements (eg box) even though this is not a dashboard
              
              tags$title("JHEEM - Ending HIV"),
              
              # Add js scripts to shinyjs
              shinyjs::useShinyjs(),
              extendShinyjs(script = 'sounds.js', functions = c('chime', 'chime_if_checked')),
              extendShinyjs(script = 'window_sizes.js', functions = c('ping_display_size', 'ping_display_size_onload', 'set_input_value')),
              extendShinyjs(script = 'download_plotly.js', functions = c('download_plotly')),
              extendShinyjs(script = 'accordion.js', functions = c('trigger_accordion')),
              
              # Add CSS Files
              tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "css/main_layout.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "display_panel.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "custom_controls.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "plot_controls.css"),
                  #tags$link(rel = "stylesheet", type = "text/css", href = "box_colors.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "color_schemes/color_scheme_jh.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "accordion.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "css/chevrons.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "css/errors.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "notifications.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "css/about.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "css/overview.css"),
                  tags$link(rel = "stylesheet", type = "text/css", href = "css/contact.css"),
                  
                  #   tags$script(src = 'window_height.js'),
                  tags$script(src = 'window_sizes.js'),
                  tags$script(src = 'accordion.js'),
                  tags$script(src = 'setup_tooltips.js'),
                  tags$script(src = 'box_expansion.js'),
                  tags$script(src = 'copy_to_clipboard.js'),
              ),
              
              # Other R dependencies
              useShinyalert(),
              
              tags$body(style='height:100%;',
                        navbarPage(
                            id='main_nav',
                            title=app.title,
                            collapsible=F,
                            selected = selected.tab,
                            
                            
                            tabPanel(
                                title = 'Overview',
                                value = 'overview',
                                make.tab.popover("overview", title=OVERVIEW.POPOVER.TITLE, content=OVERVIEW.POPOVER),
                                make.web.version.field(query.settings$web.version.data), #store the web version here
                                includeHTML(file.path('html_pages', query.settings$web.version.data$overview.page))
                            ),
                            tabPanel(
                                title = paste0('Pre-Run ', query.settings$web.version.data$intervention.label, "s"),
                                value = 'prerun_interventions',
                                make.tab.popover("prerun_interventions", 
                                                 title=get.prerun.popover.title(query.settings$web.version.data), 
                                                 content=get.prerun.popover(query.settings$web.version.data)),
                                make.prerun.content(location=query.settings$location,
                                                    web.version.data=query.settings$web.version.data,
                                                    intervention.selector.panel=INTERVENTION.SELECTOR.PANELS[[query.settings$web.version.data$name]])
                            ),
                            tabPanel(
                                title = paste0('Custom ', query.settings$web.version.data$intervention.label, "s"),
                                value = 'custom_interventions',
                                make.tab.popover("custom_interventions", 
                                                 title=get.custom.popover.title(query.settings$web.version.data), 
                                                 content=get.custom.popover(query.settings$web.version.data)),
                                make.custom.content(location=query.settings$location,
                                                    web.version.data=query.settings$web.version.data)
                            ),
                            tabPanel(
                                "FAQ",
                                value='faq',
                                make.tab.popover("faq", title=FAQ.POPOVER.TITLE, content=FAQ.POPOVER),
                                includeHTML('html_pages/faq.html')
                            ),
                            tabPanel(
                                "About the JHEEM",
                                value='about_the_jheem',
                                make.tab.popover("about_the_jheem", title=ABOUT.POPOVER.TITLE, content=ABOUT.POPOVER),
                                includeHTML('html_pages/about.html')
                            ),
                            tabPanel(
                                "Our Team",
                                value='our_team',
                                make.tab.popover("our_team", title=OUR.TEAM.POPOVER.TITLE, content=OUR.TEAM.POPOVER),
                                includeHTML('html_pages/team.html')
                            ),
                            tabPanel(
                                "Contact Us",
                                value='contact_us',
                                make.tab.popover("contact_us", title=CONTACT.POPOVER.TITLE, content=CONTACT.POPOVER),
                                CONTACT.CONTENT
                            )
                        )  # </navbarPage>
              ) #</body>
    )
}
