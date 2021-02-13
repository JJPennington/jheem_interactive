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
     selectInput.label,
     selectInput(
       inputId=paste0(inputId.prefix, '_value', i),
       label='',
       # label=selectInput.label,
       choices=choices
       # selected=choices[1]
    )))  # </selectInput/conditionalPanel/fluidRow>
  )  # /div>
}

subpopBox <- function(i) {
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
   
   # conditionalPanel(
   #   condition=paste0(
   #     # '(input.customIntervention_box_switch', i, ' == true)'),
   #     '(input.selected_subpop == ', i, ')'
   #   ),
    # box(
    #   title=paste0("Subpopulation ", i),
    #   # collapsible=TRUE,
    #   # collapsed=collapse,
    #   status="primary",
    #   width=NULL,
    #   solidHeader=TRUE,
     wellPanel(
       HTML('<h4>Subpop ', i,' demographics</h4>'),
       lapply(dimension.value.options,
           function(dim) {
             switchId = paste0(dim[['name']], '_switch', i)
             label.plural = paste0(tolower(dim[['label']]), 's')
             if (dim[['name']] == 'sex')
               label.plural = 'sexes'
             materialSwitch.value = FALSE
             if (dim[['name']] == 'age-groups')
               materialSwitch.value = TRUE
             
             fluidRow(
               fluidRow(
                HTML(paste0('<b>', dim[['label']], '</b>'))
               ),  # </fluidRow>
               fluidRow(
                materialSwitch(
                 inputId=switchId,
                 # label='Select all',
                 label=paste0('Include all ', label.plural),
                 value=materialSwitch.value,
                 right=TRUE,
                 status='primary'),
                # Got 'undefined' error for input val, prolly cuz hadnt loaded
                # conditionalPanel(
                  # condition=paste0('input.', switchId, ' == false'),
                  checkboxGroupInput(
                    inputId=paste0(dim[['name']], i),
                    label=NULL,
                    choiceNames=unname(dim[['choices']]),
                    choiceValues=names(dim[['choices']]) )                  
                # )
               ),  # </fluidRow>
             verticalSpacer(10)
             
       )})  # </fluidRow/lapply>
   # ))  # </conditionalPanel/Box>
   )  # </wellPanel>
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
   
   selectInput(
     inputId='n_subpops',
     label='Number of subpopulations to include',
     choices=c(1:5)),
   
   HTML('<strong>Edit subpopulations:</strong>'),
   tabsetPanel(
     id='subpop_tabset_panel',
     type='pills',
     tabPanel(
       title=1,
       subpopBox(1)),
     tabPanel(
       title=2,
       subpopBox(2)),
     tabPanel(
       title=3,
       subpopBox(3)),
     tabPanel(
       title=4,
       subpopBox(4)),
     tabPanel(
       title=5,
       subpopBox(5))
   ),
   
   # map(
   #   1:5,
   #   function(i) subpopBox(i) )
   
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
              '(input.subpop_tabset_panel ==', i, ')'
            ),
            wellPanel(
              HTML('<h4>Subpop ', i,' intervention details</h4>'),
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
                  # '<b>Include testing in intervention</b>'),
                  '<b>Include testing</b>'),
                selectInput.label='Frequency of testing
        (individuals in targeted subgroups are tested,
        on average, once every so many months)',
                choices=c(
                  '3 months'=3, 
                  '6 months'=6, 
                  '12 months'=12, 
                  '24 months'=24)),
              conditionalDropdown(
                i=i,
                inputId.prefix='prep',
                # box.title=div(icon('pills'), 'Pre-Exposure Prophylaxis (PrEP)'),
                #alt icon: prescription-bottle-alt
                materialSwitch.label=HTML(
                  # '<b>Include PrEP in Intervention</b>'),
                  '<b>Include PrEP</b>'),
                selectInput.label='Proportion on PrEP: (this percentage of
        individuals in targeted subgroups are prescribed and
        adherent to PrEP with HIV screening every 3mo)',
                choices=c(
                  '10%'=0.1, 
                  '25%'=0.25, 
                  '15%'=0.15, 
                  '75%'=0.75)),
              conditionalDropdown(
                i=i,
                inputId.prefix='viral_suppression',
                # box.title=div(icon('virus'), 'Viral Suppression'),
                materialSwitch.label=HTML(
                  # '<b>Include viral suppression in Intervention</b>'),
                  '<b>Include viral suppression</b>'),
                selectInput.label='Suppressed Proportion: (this percentage of
        PWH with diagnosed HIV in the targeted subgroups are virally
        suppressed)',
                choices=c(
                  '80%'=0.8, 
                  '85%'=0.85, 
                  '90%'=0.9, 
                  '95%'=0.95))
    ))}),  # </wellPanel/conditionalPanel/map>
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
