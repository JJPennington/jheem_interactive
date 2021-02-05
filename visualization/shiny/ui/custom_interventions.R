
CUSTOM.CONTENT = tags$table(
    class='display_table',
    tags$tr(
                   
#-- The 1st Left Panel --#
tags$td(class='controls_td',
        rowspan=2,
        tags$div(class='controls',
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
                     size=NULL)
                 )
        ),


#-- The 2nd Left Panel --#
tags$td(class='controls_td',
        tags$div(class='controls',
            "more controls here")
),

        
#-- The Main Panel --#
tags$td(class='display_td',
        rowspan=2,
        tags$div(class='display_td',
            create.display.panel('custom')        
        )
),


#-- The Right Panel --#
tags$td(class='controls_td',
        tags$div(class='controls',
            "controls")),
),

#-- The Action Buttons --#
tags$tr(
    # Left panel button
    tags$td(class='cta_td',
            actionButton(class='cta', inputId='run_custom', label='Simulate Interventions')),
    
    # Right panel button
    tags$td(class='cta_td',
            actionButton(class='cta', inputId='redraw_custom', label='Adjust Projections')),
)
)