VERSION = '1.0'

##-------------------##
##-- LIBRARY CALLS --####
##-------------------##
'EndingHIV RShiny web front-end process: UI spec'


library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(shinyBS)


source('load_resources.R')
source('plot_interface/options.R')
source('server/server_utils.R')
source('server/prerun_interface.R')

#-- HELPERS --#
source('helpers/general_helpers.R')
source('ui/ui_helpers.R')
source('ui/popovers.R')
source('ui/intervention_selector.R')
source('ui/display_helpers.R')
source('ui/styling_helpers.R')
source('server/control_helpers.R')
source('ui/custom_helpers.R')

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
ui = tags$html(style='height:100%',
  useShinydashboard(), #this call let's us use dashboard elements (eg box) even though this is not a dashboard
  
  # Add js scripts to shinyjs
  shinyjs::useShinyjs(),
  extendShinyjs(script = 'sounds.js', functions = c('chime', 'chime_if_checked', 'chime_alert')),
  extendShinyjs(script = 'onload.js', functions = c('ping_display_size')),
  
  # Add CSS Files
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "display_layout.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "display_panel.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom_controls.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "box_colors.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "color_schemes/color_scheme_grayscale.css"),
    
#    tags$link(rel = "stylesheet", type = "text/css", href = "not using/tmp.css"),
    
    tags$link(rel = "stylesheet", type = "text/css", href = "notifications.css"),
    
 #   tags$script(src = 'window_height.js'),
    tags$script(src = 'window_sizes.js'),
    tags$script(src = 'setup_tooltips.js'),
  ),
  
  tags$body(style='height:100%;',
    navbarPage(
      id='main_nav',
   #   theme='color_scheme/bootstrap.css',
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
  ) #</body>
)

