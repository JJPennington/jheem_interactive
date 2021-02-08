# Libs ####
library('purrr')

# Functions ####
metricBox <- function(
 # i,
 inputId.prefix,
 box.title,
 sliderInput.label,
 knobInput.label,
 knobInput.min,
 knobInput.max,
 knobInput.value,
 materialSwitch.label,
 knobInput.post='',
 column.width=page.width * 1/2
) {
 # column(
 #  width=column.width,
  box(
   width=NULL, 
   title=box.title,
   collapsible=TRUE,
   collapsed=TRUE,
   status='info', 
   solidHeader=TRUE,
   
   wellPanel(
    
    fluidRow(
     materialSwitch(
      # inputId=paste0(inputId.prefix, '_switch', i), 
      inputId=paste0(inputId.prefix, '_switch'), 
      label=materialSwitch.label, 
      value=T,
      right=T,
      status='primary')),  # </rowInner1/3>
    
    fluidRow(conditionalPanel(
     condition=paste0(
      # 'input.', inputId.prefix, '_switch', i, ' == true'),
      'input.', inputId.prefix, '_switch', ' == true'),
     tableRow(
      vertical.align='top',
      inner.padding='25px',
      column(
       width=12,
       align='center',
       tags$style(type="text/css", "#string { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}"),
       knobInput(
        # inputId=paste0(inputId.prefix, '_value', i),
        inputId=paste0(inputId.prefix, '_value'),
        label=knobInput.label,
        min=knobInput.min,
        max=knobInput.max,
        step=1,
        value=knobInput.value,
        pre=NULL,
        post=knobInput.post,
        cursor=FALSE,
        lineCap=c("default", "round")[1],
        rotation=c("clockwise", "anticlockwise")[1],
        # px only; doesn't seem to work
        # width='100px',  
        # height='100px', 
        angleOffset=0,
        angleArc=360,
        thickness=NULL,
        displayInput=TRUE,
        displayPrevious=FALSE,
        fgColor=NULL,
        inputColor=NULL,
        bgColor=NULL,
        fontSize=NULL,
        readOnly=FALSE,
        immediate=TRUE )) ),  # </rowInner2/3>
     
     tableRow(
      vertical.align='top',
      inner.padding='25px',
      sliderInput(
       # inputId=paste0(inputId.prefix, '_years', i),
       inputId=paste0(inputId.prefix, '_years'),
       label=sliderInput.label,
       value=c(2020, 2030),
       min=2020,
       max=2030,
       step=1))  # </rowInner3/3>
    ))  # </conditionalPanel /fluidRow>
   ))  # </wellPanel /box 
  # )  # /column>
}

customInterventionBox <- function(i) {
 collapse = TRUE
 customIntervention_box_switch.value = TRUE
 if (i == 1) {
  collapse = FALSE
  customIntervention_box_switch.value = TRUE
 }
 
 dimension.value.options = get.dimension.value.options(
  version=version,
  location=input$geographic_location,
  msm_idu_mode=TRUE)
 dimension.value.col.width = 12 / length(dimension.value.options)
 
 # conditionalPanel(
 #   condition=paste0(
 #  '(', as.character(i), ' == 1)',
 #  ' || ',
 #  '(input.group_addition_checkbox_', 
 #  as.character(as.numeric(i) - 1), 
 #  '== true)'), 
 
 # fluidRow(
 #  column(
 #   width=page.width,
   box(
    title=paste0("Subpopulation ", i),
    collapsible=TRUE,
    collapsed=collapse,
    status="primary",
    width=NULL,
    solidHeader=TRUE,
    
    # Row 0.1/2: Activate 
    tableRow(
     inner.padding='25px',
     materialSwitch(
      inputId=paste0(
       'customIntervention_box_switch', i), 
      label=paste0('Use subpopulation ', i, ' ?'), 
      value=customIntervention_box_switch.value,
      right=F,
      status='primary')
    ),
    
    conditionalPanel(
     # condition=state()[[paste0(
     #   'customIntervention_box_switch', i)]] == TRUE,
     condition=paste0(
      '(input.customIntervention_box_switch', i, ' == true)'),
     # fluidRow(
     #  column(
     #   width=page.width,
     #   box(
     #    width=NULL, 
     #    title=tags$div(icon('user-friends'), "Targeted Subgroup(s)"),
     #    collapsible=F,
     #    collapsed=F,
     #    status="success", 
     #    solidHeader=TRUE,
     #    
     #    tableRow(
         lapply(dimension.value.options,
             function(dim) {
               fluidRow(
              # column(
               # align='center',
               # tags$style(type="text/css", "#string { height: 50px; width: 100%; display: block; padding-left: 20px; }"),
               # width=dimension.value.col.width,
               
               fluidRow(
                HTML(paste0('<b>', dim[['label']], '</b>'))
               ),  # </fluidRow>
               fluidRow(
                # tableRow(
                #   vertical.align='top',
                #   inner.padding='25px',
                
                materialSwitch(
                 inputId=paste0(
                  dim[['name']], '_switch', i),
                 label='Select all',
                 value=FALSE,
                 right=TRUE,
                 status='primary'),
                # checkboxInput(
                #   inputId=paste0(
                #  dim[['name']], '_switch', i),
                #   label='Select all',
                #   value=FALSE),
                
                # ),  # </fluidRow>
                # fluidRow(
                checkboxGroupInput(
                 inputId=paste0(dim[['name']], i),
                 # label=dim[['label']],
                 label=NULL,
                 # selected=state()[[
                 #  paste0(dim[['name']], i)]],
                 # selected=names(dim[['choices']]),
                 choiceNames=unname(dim[['choices']]),
                 choiceValues=names(dim[['choices']]) )
               ),  # </fluidRow>
               verticalSpacer(10)
              # )  # </column>
               )
            })  # </lapply>
        # ))))
   ))
  # ))
}

# UI ####
CUSTOM.CONTENT = tags$table(
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
 
 map(
   1:5,
   function(i) customInterventionBox(i) )
 )
),  # </leftPanel>

#-- The 2nd Left Panel --####
tags$td(
  class='controls_td',
  tags$div(
    class='controls',
           
  # fluidRow(
   metricBox(
     # i=i,
     inputId.prefix='test_freq_months',
     box.title=div(icon('vial'), "HIV Testing"),
     materialSwitch.label=HTML(
       '<b>Include testing in intervention</b>'),
     knobInput.label='Frequency of testing
  (individuals in targeted subgroups are tested,
  on average, once every so many months)',
     knobInput.min=1,
     knobInput.max=24,
     knobInput.value=12,
     knobInput.post='mo',
     # knobInput.post='',
     sliderInput.label='Years from when testing
            intervention begins to when it is fully
            implemented',
     column.width=page.width * 1/3),

   metricBox(
     # i=i,
     inputId.prefix='prep',
     box.title=div(icon('pills'), 'Pre-Exposure Prophylaxis (PrEP)'),
     #alt icon: prescription-bottle-alt
     materialSwitch.label=HTML(
       '<b>Include PrEP in Intervention</b>'),
     knobInput.label='Proportion on PrEP: (this percentage of
  individuals in targeted subgroups are prescribed and
  adherent to PrEP with HIV screening every 3mo)',
     knobInput.min=1,
     knobInput.max=100,
     knobInput.value=25,
     knobInput.post='%',
     sliderInput.label='Years from when PrEP intervention
  begins to when it is fully implemented',
     column.width=page.width * 1/3),

   metricBox(
     # i=i,
     inputId.prefix='viral_suppression',
     box.title=div(icon('virus'), 'Viral Suppression'),
     materialSwitch.label=HTML(
       '<b>Include viral suppression in Intervention</b>'),
     knobInput.label='Suppressed Proportion: (this percentage of
  PWH with diagnosed HIV in the targeted subgroups are virally
  suppressed)',
     knobInput.min=1,
     knobInput.max=100,
     knobInput.value=80,
     knobInput.post='%',
     sliderInput.label='Years from when suppression intervention
  begins to when it is fully implemented',
     column.width=page.width * 1/3)
),
# )),
  
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
