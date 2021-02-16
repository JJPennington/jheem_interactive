PRERUN.CONTENT = tags$table(class='display_table', tags$tbody(class='display_tbody',
    
##-- HEADERS AND DISPLAY --##  
tags$tr(
    #-- Left Header --#
    tags$td(class='controls_header_td controls_narrow header_color',
            "Select Intervention"),
    
    #-- The Main Panel --#
    tags$td(class='display_td display_wide content_color',
            rowspan=4,
            tags$div(class='display',
                     create.display.panel('prerun')        
            )),
    
    #-- Right Header --#
    tags$td(class='controls_header_td controls_narrow header_color',
            "Figure Configuration")
), #</tr>

##-- CONTROL PANELS --##
tags$tr(
    
    #-- The Left Panel --#
    tags$td(class='controls_td controls_narrow controls_color',
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
                     
                     make.popover('location_prerun',
                                  title='What City to Project Interventions For',
                                  content="Choose from among the 32 Metropolitan Statistical Areas encompassing the 48 high-burden counties and Washington DC identified by the Ending the HIV Epidemic Initiative.",
                                  placement='right'),
                     
                     create.intervention.selector.panel('prerun')
            )),
    
    #-- The Right Panel --#
    tags$td(class='controls_td controls_narrow controls_color',
            rowspan=2,
            create.plot.control.panel('prerun')
    )
    
), #</tr>

##-- CTA TEXT --##
tags$tr(
    
    #-- Left panel text --#
    tags$td(class='cta_text_td controls_narrow cta_background_color',
            HTML("This will take 10-30 seconds<BR>
                  <input type='checkbox' id='chime_run_prerun' name='chime_run_prerun' style='float: left'>
                  <label for='chime_run_prerun'>&nbsp;Play a chime when done</label>")
        #         tags$div(class='checkbox_wrapper',
         #                 checkboxInput(inputId = 'chime_run_prerun',
          #                     "Play a chime when done"))
    ),
    
), #</tr>

##-- CTA BUTTONS --##
tags$tr(
    
    #-- Left panel button --#
    tags$td(class='cta_td controls_narrow cta_color',
            actionButton(class='cta cta_color', inputId='run_prerun', label='Generate Projections')
            ),
    
    #-- Right panel button --#
    tags$td(class='cta_td controls_narrow cta_color',
            actionButton(class='cta cta_color', inputId='redraw_prerun', label='Adjust Projections')
            )
    
) #</tr>

)) #</tbody></table>