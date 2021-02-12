VERSION = '1.0'

##-------------------##
##-- LIBRARY CALLS --####
##-------------------##
'EndingHIV RShiny web front-end process: UI spec'


library(shinydashboard)
library(shinyjs)
library(shinyWidgets)

library(shinyBS)

source('load_resources.R')
source('plot_interface/options.R')
source('server/server_utils.R')
source('server/prerun_interface.R')

#-- HELPERS --#
source('server/general_helpers.R')
source('ui/intervention_selector.R')
source('ui/display_helpers.R')
source('ui/styling_helpers.R')
source('server/control_helpers.R')
source('ui/popovers.R', local=T)

#-- MAIN TAB CONTENT FILES --#
source('ui/contact.R', local=T)
source('ui/prerun_interventions.R', local=T)
source('ui/custom_interventions.R', local=T)


##------------------##
##-- DEFINE the UI--####
##------------------##
# Variables
app.title = "JHEEM: Ending HIV in the US"




# UI
ui = tagList(
  useShinydashboard(), #this call let's us use dashboard elements (eg box) even though this is not a dashboard
  
  # Add js scripts to shinyjs
  shinyjs::useShinyjs(),
#  extendShinyjs('www/menu_helpers.js', functions=c('testAlert')),
  
  # Add CSS Files
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "display_layout.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "display_panel.css"),
#    BOX.TAGS,
    tags$link(rel = "stylesheet", type = "text/css", href = "box_colors.css"),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "notifications.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "joe.css"),
    tags$script(src = 'setup_tooltips.js')
  ),
  
  navbarPage(
    id='main_nav',
    title=app.title,
    # header=tags$div(),
    # footer=tags$div(),
    collapsible=F,
    tabPanel(
      title = 'Pre-Run Interventions',
      make.tab.popover("prerun_interventions", title=PRERUN.POPOVER.TITLE, content=PRERUN.POPOVER),
      PRERUN.CONTENT
    ),
    tabPanel(
      title = "Custom Interventions",
      make.tab.popover("custom_interventions", title=CUSTOM.POPOVER.TITLE, content=CUSTOM.POPOVER),
      CUSTOM.CONTENT
    ),
    tabPanel(
      "About the Model",
      uiOutput("about")),
    tabPanel(
      "FAQ",
      uiOutput("faq")),
    tabPanel(
      "Contact Us",
      CONTACT.CONTENT
    )
  )  # </navbarPage>
)

