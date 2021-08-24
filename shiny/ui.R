
##-----------------##
##-- SOURCE CODE --##
##-----------------##

source('source_code.R')

#-- MAIN TAB CONTENT FILES --#
source('ui/contact.R', local=T)
source('ui/prerun_interventions.R', local=T)
source('ui/custom_interventions.R', local=T)


##------------------##
##-- DEFINE the UI--##
##------------------##
# Variables
app.title = "JHEEM: Ending HIV in the US"

# UI
ui = tags$html(style='height:100%',
               
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
     
     tabPanel(
         title = 'Overview',
         value = 'overview',
         make.tab.popover("overview", title=OVERVIEW.POPOVER.TITLE, content=OVERVIEW.POPOVER),
         includeHTML('html_pages/overview.html')
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

