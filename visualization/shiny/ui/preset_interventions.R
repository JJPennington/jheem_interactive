

PRESET.CONTENT = tags$table(
    class='display_table',
    tags$tr(
                   
#-- The Left Panel --#
tags$td(class='controls',
        selectInput(
            inputId="location_preset", 
            # label=NULL,
            label="Location",
            choices=invert.keyVals(get.preset.locations(version=VERSION)),
            # selected=location.choice,
            selected=NULL,
            multiple=FALSE,
            selectize=TRUE, 
            width=NULL, 
            size=NULL),
        create.intervention.selector.panel('preset')
        ),


        
#-- The Main Panel --#
tags$td(class='display',
        rowspan=2,
        create.display.panel('preset')        
        ),


#-- The Right Panel --#
tags$td(class='controls',
        "controls")

),

#-- The Action Buttons --#
tags$tr(
    tags$td(class='cta_holder',
            actionButton(class='cta', inputId='run_preset', label='Generate Projections')),
    tags$td('button will be here')
)
)
