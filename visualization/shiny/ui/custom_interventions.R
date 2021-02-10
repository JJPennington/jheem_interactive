# Libs ####
library('purrr')

# Functions ####
conditionalDropdown <- function(
 i,
 inputId.prefix,
 # box.title,
 selectInput.label,
 materialSwitch.label,
 choices
) {
  div(
    fluidRow(
     materialSwitch(
      inputId=paste0(inputId.prefix, '_switch', i),
      label=materialSwitch.label, 
      value=FALSE,
      right=TRUE,
      status='primary'
    )),  # </materialSwitch/fluidRow>
    fluidRow(conditionalPanel(
     condition=paste0(
      'input.', inputId.prefix, '_switch', i, ' == true'
      # '(input.selected_subpop == ', as.character(i), ')'
      ),
      selectInput(
        inputId=paste0(inputId.prefix, '_value', i),
        label=selectInput.label,
        choices=choices
        # selected=choices[1]
    )))  # </selectInput/conditionalPanel/fluidRow>
  )  # /div>
}

customInterventionBox <- function(i) {
 # collapse = TRUE
 # customIntervention_box_switch.value = FALSE
 # if (i == 1) {
 #  # collapse = FALSE
 #  customIntervention_box_switch.value = TRUE
 # }
 
 dimension.value.options = get.dimension.value.options(
  version=version,
  location=input$geographic_location,
  msm_idu_mode=TRUE)
 
 fluidRow(
   # materialSwitch(
   #  inputId=paste0(
   #   'customIntervention_box_switch', i), 
   #  label=paste0('Use subpopulation ', i, ' ?'), 
   #  value=customIntervention_box_switch.value,
   #  right=F,
   #  status='primary'),
   
   conditionalPanel(
     condition=paste0(
       # '(input.customIntervention_box_switch', i, ' == true)'),
       '(input.selected_subpop == ', i, ')'
     ),
    box(
      title=paste0("Subpopulation ", i),
      # collapsible=TRUE,
      # collapsed=collapse,
      status="primary",
      width=NULL,
      solidHeader=TRUE,
       lapply(dimension.value.options,
           function(dim) {
             
             fluidRow(
               fluidRow(
                HTML(paste0('<b>', dim[['label']], '</b>'))
               ),  # </fluidRow>
               fluidRow(
                materialSwitch(
                 inputId=paste0(
                  dim[['name']], '_switch', i),
                 label='Select all',
                 value=FALSE,
                 right=TRUE,
                 status='primary'),
                checkboxGroupInput(
                 inputId=paste0(dim[['name']], i),
                 label=NULL,
                 choiceNames=unname(dim[['choices']]),
                 choiceValues=names(dim[['choices']]) )
               ),  # </fluidRow>
             verticalSpacer(10)
             
       )})  # </fluidRow/lapply>
   ))  # </conditionalPanel/Box>
  )  # </fluidRow>
}

# UI ####
CUSTOM.CONTENT = tags$table(
# get.custom_content = function(input) {
 # renderUI({
  tags$table(
   class='display_table',
   tags$tr(
         
  #-- The 1st Left Panel --####
  tags$td(
  class='controls_td',
  rowspan=2,
  tags$div(
   class='controls',
   
   selectInput(
    inputId="location_custom", 
    # label=NULL,
    label="Location",
    choices=invert.keyVals(get.prerun.locations(version=VERSION)),
    # selected=location.choice,
    selected=NULL,
    multiple=FALSE,
    selectize=TRUE, 
    width=NULL, 
    size=NULL),
   
   sliderInput(
     inputId='n_subpops',
     label='Number of subpopulations',
     min=1,
     max=5,
     value=1,
     step=1),
   
   selectInput(
     inputId='selected_subpop',
     label='Editing subpopulation:',
     # choices=c(1:input$n_subpops)),
     # TODO
     choices=c(1:5)),
   
   map(
     1:5,
     function(i) customInterventionBox(i) )
   
   )  # </div>
  ),  # </leftPanel>
  
  #-- The 2nd Left Panel --####
  tags$td(
    class='controls_td',
    tags$div(
      class='controls',
      map(
        1:5, function(i) {
          conditionalPanel(
            condition=paste0(
              # '(input.customIntervention_box_switch', i, ' == true)'
              '(input.selected_subpop == ', i, ')'
            ),
            sliderInput(
              inputId=paste0('custom_interventions_years', i),
              label='Years',
              value=c(2020, 2030),
              min=2020,
              max=2030,
              step=1),
            conditionalDropdown(
              i=i,
              inputId.prefix='test_freq_months',
              # box.title=div(icon('vial'), "HIV Testing"),
              materialSwitch.label=HTML(
                '<b>Include testing in intervention</b>'),
              selectInput.label='Frequency of testing
      (individuals in targeted subgroups are tested,
      on average, once every so many months)',
              choices=c(3, 6, 12, 24)),
            conditionalDropdown(
              i=i,
              inputId.prefix='prep',
              # box.title=div(icon('pills'), 'Pre-Exposure Prophylaxis (PrEP)'),
              #alt icon: prescription-bottle-alt
              materialSwitch.label=HTML(
                '<b>Include PrEP in Intervention</b>'),
              selectInput.label='Proportion on PrEP: (this percentage of
      individuals in targeted subgroups are prescribed and
      adherent to PrEP with HIV screening every 3mo)',
              choices=c(10, 25, 15, 75)),
            conditionalDropdown(
              i=i,
              inputId.prefix='viral_suppression',
              # box.title=div(icon('virus'), 'Viral Suppression'),
              materialSwitch.label=HTML(
                '<b>Include viral suppression in Intervention</b>'),
              selectInput.label='Suppressed Proportion: (this percentage of
      PWH with diagnosed HIV in the targeted subgroups are virally
      suppressed)',
              choices=c(80, 85, 90, 95))
      )}),  # </conditionalPanel/map>
  ),
  #-- The Main Panel --####
  tags$td(class='display_td',
    rowspan=2,
    tags$div(class='display_td',
     create.display.panel('custom')  
    )
  ),
  
  
  #-- The Right Panel --####
  tags$td(class='controls_td',
    tags$div(class='controls',
     "controls"))
  ),
  
  #-- The Action Buttons --####
  tags$tr(
   # Left panel button
   tags$td(class='cta_td',
     actionButton(class='cta', inputId='run_custom', label='Simulate Interventions')),
   
   # Right panel button
  tags$td(class='cta_td',
    actionButton(class='cta', inputId='redraw_custom', label='Adjust Projections')),
  )
  
  ))  # </tr/table>
)
#   })  # </renderUI)
# }  # </CUSTOM.CONTENT()>
