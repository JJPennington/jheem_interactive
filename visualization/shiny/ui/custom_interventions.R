
CUSTOM.CONTENT = tags$table(
    class='display_table',
    tags$tr(
                   
#-- The 1st Left Panel --#
tags$td(class='controls',
        rowspan=2,
        selectInput(
            inputId="location_custom", 
            # label=NULL,
            label="Location",
            choices=invert.keyVals(get.preset.locations(version=VERSION)),
            # selected=location.choice,
            selected=NULL,
            multiple=FALSE,
            selectize=TRUE, 
            width=NULL, 
            size=NULL),
        ),


#-- The 2nd Left Panel --#
tags$td(class='controls',
        "more controls here"
),

        
#-- The Main Panel --#
tags$td(class='display',
        rowspan=2,
        create.display.panel('custom')        
        ),


#-- The Right Panel --#
tags$td(class='controls',
        "controls")

),

#-- The Action Buttons --#
tags$tr(
    tags$td('button will be here'),
    tags$td('button will be here')
)

)
