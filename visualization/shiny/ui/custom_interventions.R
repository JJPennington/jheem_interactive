
CUSTOM.CONTENT = tags$table(class='display_table', tags$tbody(class='display_tbody',
    
##-- HEADERS AND DISPLAY --##  
tags$tr(
    #-- Left Header --#
    tags$td(class='controls_header_td controls_wide header_color',
            "Specify Intervention"),
    
    #-- The Main Panel --#
    tags$td(class='display_td display_narrow content_color',
            rowspan=4,
            tags$div(class='display',
                     create.display.panel('custom')        
            )),
    
    #-- Right Header --#
    tags$td(class='controls_header_td controls_narrow header_color',
            "Figure Configuration")
), #</tr>

##-- CONTROL PANELS --##
tags$tr(
    
    #-- The Left Panel --#
    tags$td(class='controls_td controls_wide controls_color',
            tags$div(class='controls',
                     tags$div(id='location_custom_holder',
                              inline.select.input(inputId='location_custom',
                                                  label='Location: ',
                                                  choices=invert.keyVals(get.prerun.locations(version=VERSION)))
                     ),
                     
                     make.popover('location_custom_holder',
                                  title='What City to Simulate Interventions For',
                                  content="Choose from among the 32 Metropolitan Statistical Areas encompassing the 48 high-burden counties and Washington DC identified by the Ending the HIV Epidemic Initiative.",
                                  placement='right'),
                     
                     tags$div(id='n_subpop_panel',
                              inline.select.input(inputId='n_subpops',
                                                  label='How Many Distinct Subgroups to Target Interventions To:',
                                                  choices=1:MAX.N.SUBPOPULATIONS,
                                                  width='60px',
                                                  selectize=F)
                     ),
                     make.popover('n_subpop_panel',
                                  title="Number of Target Subgroups",
                                  content="Select the number of different subgroups to which you want to deliver an intervention. Once you have chosen the number, specify the details for each subgroup below.",
                                  placement='right'),
                     
                     HTML('<strong>Specify Intervention for Subgroup:</strong>'),
                              
                     do.call(tabsetPanel,
                             c(list(id='custom_tabset_panel', type='pills'),
                               lapply(1:MAX.N.SUBPOPULATIONS, function(i){
                                   tabPanel(title=i,  
                                            value=i,
                                            wellPanel(style = "padding: 10px; padding-right:5px",
                                                      tags$table(class='specify_custom',
                                                                 tags$tr(
                                                                     tags$th(paste0("Subgroup ", i, " Characteristics:")),
                                                                     tags$th("Intervention Details:")
                                                                 ),
                                                                 tags$tr(
                                                                     tags$td(create.custom.tpop.box(i)),
                                                                     tags$td(create.custom.intervention.unit.selector(i))
                                                                 )
                                                      )
                                            ) #</wellPanel>
                                   ) #</tabPanel>
                               }))
                             ) #</tabsetPanel>

            ) #</div class=controls>
    ),  # </td>
    
    #-- The Right Panel --#
    tags$td(class='controls_td controls_narrow controls_color',
            rowspan=2,
            create.plot.control.panel('custom')
    )
    
), #</tr>

##-- CTA TEXT --##
tags$tr(
    
    #-- Left panel text --#
    tags$td(class='cta_text_td controls_wide cta_background_color',
            HTML("This will take 2-5 minutes<BR>
                  <input type='checkbox' id='chime_run_custom' name='chime_run_custom' style='float: left'>
                  <label for='chime_run_custom'>&nbsp;Play a chime when done</label>")
    ),
    
), #</tr>

##-- CTA BUTTONS --##
tags$tr(
    
    #-- Left panel button --#
    tags$td(class='cta_td controls_wide cta_color',
            actionButton(class='cta cta_color', inputId='run_custom', label='Simulate Intervention')
            ),
    
    #-- Right panel button --#
    tags$td(class='cta_td controls_narrow cta_color',
            actionButton(class='cta cta_color', inputId='redraw_custom', label='Adjust Projections')
            )
    
) #</tr>

)) #</tbody></table>
