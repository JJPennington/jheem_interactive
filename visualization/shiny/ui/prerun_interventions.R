

PRERUN.CONTENT = tags$table(
    class='display_table',
    tags$tr(
                   
#-- The Left Panel --#
tags$td(class='controls_td',
    tags$div(class='controls',
        selectInput(
            inputId="location_prerun", 
            # label=NULL,
            label="Location",
            choices=invert.keyVals(get.prerun.locations(version=VERSION)),
            # selected=location.choice,
            selected=NULL,
            multiple=FALSE,
            selectize=TRUE, 
            width=NULL, 
            size=NULL),
        create.intervention.selector.panel('prerun')
        )),


        
#-- The Main Panel --#
tags$td(class='display_td',
        rowspan=2,
        tags$div(class='display',
            create.display.panel('prerun')        
        )),


#-- The Right Panel --#
tags$td(class='controls_td',
        tags$div(class='controls',
        "controls"))

),

#-- The Action Buttons --#
tags$tr(
    # Left panel button
    tags$td(class='cta_td',
            actionButton(class='cta', inputId='run_prerun', label='Generate Projections')),
    
    # Right panel button
    tags$td(class='cta_td',
            actionButton(class='cta', inputId='redraw_prerun', label='Adjust Projections')),
)
)
