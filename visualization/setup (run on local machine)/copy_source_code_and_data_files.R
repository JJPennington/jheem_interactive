
RESOURCE.DIR = 'visualization/shiny/resources/'
CODE.DIR = 'visualization/shiny/model_code/'
SRC.DIR = '../Ending_HIV'
OVERWRITE.RESOURCES = F

# Resources
file.copy(file.path(SRC.DIR, 'cached/msa.surveillance.Rdata'), file.path(RESOURCE.DIR, 'msa_surveillance.Rdata'), overwrite=OVERWRITE.RESOURCES)
file.copy(file.path(SRC.DIR, 'cached/state.surveillance.Rdata'), file.path(RESOURCE.DIR, 'state_surveillance.Rdata'), overwrite=OVERWRITE.RESOURCES)
file.copy(file.path(SRC.DIR, 'cached/DEFAULT.LOCALE.MAPPING.Rdata'), file.path(RESOURCE.DIR, 'locale_mapping.Rdata'), overwrite=OVERWRITE.RESOURCES)
file.copy(file.path(SRC.DIR, 'cached/census_totals.Rdata'), file.path(RESOURCE.DIR, 'census_totals.Rdata'), overwrite=OVERWRITE.RESOURCES)

# Source Files
file.copy(file.path(SRC.DIR, 'code/visualization/plot_simulations.R'), file.path(CODE.DIR, 'plot_simulations.R'), overwrite = T)
file.copy(file.path(SRC.DIR, 'code/systematic_calibration/postprocessing.R'), file.path(CODE.DIR, 'postprocessing.R'), overwrite = T)
file.copy(file.path(SRC.DIR, 'code/data_managers/locale_mappings.R'), file.path(CODE.DIR, 'locale_mappings.R'), overwrite = T)
file.copy(file.path(SRC.DIR, 'code/data_managers/hiv_surveillance_manager.R'), file.path(CODE.DIR, 'hiv_surveillance_manager.R'), overwrite = T)
file.copy(file.path(SRC.DIR, 'code/data_managers/census_totals.R'), file.path(CODE.DIR, 'census_totals.R'), overwrite = T)
file.copy(file.path(SRC.DIR, 'code/setup/setup_jheem_from_components.R'), file.path(CODE.DIR, 'setup_jheem_from_components.R'), overwrite = T)
file.copy(file.path(SRC.DIR, 'code/setup/interpolating.R'), file.path(CODE.DIR, 'interpolating.R'), overwrite = T)
file.copy(file.path(SRC.DIR, 'code/systematic_calibration/file_manager.R'), file.path(CODE.DIR, 'file_manager.R'), overwrite=T)

file.copy(file.path(SRC.DIR, 'code/setup/default_jheem_settings.R'), file.path(CODE.DIR, 'default_jheem_settings.R'), overwrite=T)

file.copy(file.path(SRC.DIR, 'code/interventions/intervention_units.R'), file.path(CODE.DIR, 'intervention_units.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/interventions/target_population.R'), file.path(CODE.DIR, 'target_population.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/interventions/interventions.R'), file.path(CODE.DIR, 'interventions.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/interventions/intervention_presets.R'), file.path(CODE.DIR, 'intervention_presets.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/interventions/create_standard_intervention_presets.R'), file.path(CODE.DIR, 'create_standard_intervention_presets.R'), overwrite=T)

# For running interventions
file.copy(file.path(SRC.DIR, 'code/interventions/interventions_for_simset.R'), file.path(CODE.DIR, 'interventions_for_simset.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/setup/interpolating.R'), file.path(CODE.DIR, 'interpolating.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/setup/setup_helpers.R'), file.path(CODE.DIR, 'setup_helpers.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/setup/logit_transformations.R'), file.path(CODE.DIR, 'logit_transformations.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/setup/setup_jheem_from_components.R'), file.path(CODE.DIR, 'setup_jheem_from_components.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/setup/setup_jheem_components.R'), file.path(CODE.DIR, 'setup_jheem_components.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/visualization/compression.R'), file.path(CODE.DIR, 'compression.R'), overwrite=T)

file.copy(file.path(SRC.DIR, 'code/calibration/calibrated_parameters_113_helpers.R'), file.path(CODE.DIR, 'calibrated_parameters_113_helpers.R'), overwrite=T)

file.copy(file.path(SRC.DIR, 'code/data_managers/prep_manager_2.R'), file.path(CODE.DIR, 'prep_manager_2.R'), overwrite=T)
file.copy(file.path(SRC.DIR, 'code/data_managers/pairing_manager.R'), file.path(CODE.DIR, 'pairing_manager.R'), overwrite=T)
