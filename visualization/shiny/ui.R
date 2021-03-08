VERSION = '1.0'

##-------------------##
##-- LIBRARY CALLS --####
##-------------------##
'EndingHIV RShiny web front-end process: UI spec'

library('shinyalert')
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(shinyBS)

#-- THEME --#
source('ui/theme.R', local=T)

#-- RESOURCES --#
source('load_resources.R')
source('cache/load_from_cache.R')

source('plot_interface/options.R')
source('server/server_utils.R')
source('server/simulation_storage.R')
source('server/prerun_interface.R')

#-- HELPERS --#
source('helpers/display_size.R')
source('helpers/general_helpers.R')
source('helpers/accordion.R')
source('ui/ui_helpers.R')
source('ui/popovers.R')
source('ui/intervention_selector.R')
source('ui/display_helpers.R')
source('server/control_helpers.R')
source('ui/custom_helpers.R')

#-- MAIN TAB CONTENT FILES --#
source('ui/contact.R', local=T)
source('ui/prerun_interventions.R', local=T)
source('ui/custom_interventions.R', local=T)

##------------------##
##-- DEFINE the UI--##
##------------------##
# Variables
app.title = ''#"JHEEM: Ending HIV in the US"

# UI
ui = tags$html(style='height:100%',
               
useShinydashboard(), #this call let's us use dashboard elements (eg box) even though this is not a dashboard

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
     #theme=create.theme(),
     title=app.title,
     collapsible=F,
     
     tabPanel(
         title = 'Overview',
         value = 'overview',
         make.tab.popover("overview", title=OVERVIEW.POPOVER.TITLE, content=OVERVIEW.POPOVER),
         includeHTML('ui/overview.html')
     ),
     tabPanel(
         title = 'Pre-Run Interventions',
         value = 'prerun_interventions',
         make.tab.popover("prerun_interventions", title=PRERUN.POPOVER.TITLE, content=PRERUN.POPOVER),
         PRERUN.CONTENT
     ),
     tabPanel(
         title = "Custom Interventions",
         value = 'custom_interventions',
         make.tab.popover("custom_interventions", title=CUSTOM.POPOVER.TITLE, content=CUSTOM.POPOVER),
         CUSTOM.CONTENT
     ),
     tabPanel(
         "About the JHEEM",
         value='about_the_jheem',
         make.tab.popover("about_the_jheem", title=ABOUT.POPOVER.TITLE, content=ABOUT.POPOVER),
         includeHTML('ui/about.html')
     ),
     tabPanel(
         "Our Team",
         value='our_team',
         make.tab.popover("our_team", title=OUR.TEAM.POPOVER.TITLE, content=OUR.TEAM.POPOVER),
         includeHTML('ui/team.html')
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

