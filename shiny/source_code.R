VERSION = '1.0'

##-- LIBRARY CALLS --##

library(shiny)
library(shinyalert)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(DT)
library(shinyBS)
library(mailR)
library(shinycssloaders)

#-- RESOURCES --#
source('load_resources.R')

#-- OTHER --#
source('cache/load_from_cache.R')
source('simulation/simulate_intervention.R')
source('links/link_interface.R')


#-- MASTER SETTINGS --#
source('master_settings/options.R')


#-- PLOT INTERFACE --#
source('plot_interface/generate_plot.R')
source('plot_interface/plot_interface.R')


#-- HELPERS --#
source('helpers/display_size.R')
source('helpers/general_helpers.R')
source('helpers/accordion.R')
source('helpers/location_names.R')
source('helpers/progress_bar.R')
source('helpers/display_size.R')
source('helpers/error_checking.R')
source('helpers/alerts.R')

source('helpers/time_text.R')
source('helpers/multi_cache.R')
source('helpers/intervention_map.R')
source('helpers/analytics.R')


#-- SERVER FILES --##
source('server/server_utils.R')
source('server/simulation_storage.R')
source('server/prerun_interface.R')
source('server/control_helpers.R')
source('server/contact_handlers.R')
source('server/share_handlers.R')
source('server/display_event_handlers.R')


#-- UI Files --#
source('ui/ui_helpers.R')
source('ui/popovers.R')
source('ui/intervention_selector.R')
source('ui/display_helpers.R')
source('ui/custom_helpers.R')

source('ui/custom_interventions.R', local=T)
source('ui/modals.R')