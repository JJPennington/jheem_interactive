
CUSTOM.CONTENT = tags$table(class='display_table', tags$tbody(class='display_tbody',
    
##-- HEADERS AND DISPLAY --##  
tags$tr(
    #-- Left 1 Header --#
    tags$td(class='controls_header_td header_color',
            "Target Population"),
    
    #-- Left 1 Header --#
    tags$td(class='controls_header_td header_color',
            "Specify interventions"),
    
    #-- The Main Panel --#
    tags$td(class='display_narrow_td content_color',
            rowspan=4,
            tags$div(class='display',
                     create.display.panel('custom')        
            )),
    
    #-- Right Header --#
    tags$td(class='controls_header_td header_color',
            "Figure Configuration")
), #</tr>

##-- CONTROL PANELS --##
tags$tr(
    
    #-- The Left 1 Panel --#
    tags$td(class='controls_td controls_color',
            rowspan=3,
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
                         size=NULL),
                     
                     make.popover('location_custom',
                                  title='What City to Simulate Interventions For',
                                  content="Choose from among the 32 Metropolitan Statistical Areas encompassing the 48 high-burden counties and Washington DC identified by the Ending the HIV Epidemic Initiative.",
                                  placement='right'),
                     
                     tags$div(id='n_subpop_panel',
                         selectInput(
                             inputId='n_subpops',
                             label='How Many Distinct Subgroups to Target Interventions To:',
                             choices=1:MAX.N.SUBPOPULATIONS)
                         ),
                     make.popover('n_subpop_panel',
                                  title="Number of Target Subgroups",
                                  content="Select the number of different subgroups to which you want to deliver an intervention. Once you have chosen the number, specify the details for each subgroup below.",
                                  placement='right'),
                     
                     HTML('<strong>Specify Subgroup:</strong>'),
                     tags$div(id='subpop_tabset_wrapper',
                     do.call(tabsetPanel,
                             c(list(id='subpop_tabset_panel', type='pills'),
                               lapply(1:MAX.N.SUBPOPULATIONS, function(i){
                                   tabPanel(title=i, create.custom.tpop.box(i))
                               }))
                             ),
                             
                     make.popover('subpop_tabset_wrapper',
                                  title="Specify Subgroups",
                                  content="Click on the number for each subgroup and select the age(s), race(s), sex(es), and risk factor(s) to target. Once you are done, specify the intervention in the panel to the right.",
                                  placement='right')
                             
                     ), #</tabsetPanel>
            ) #</div class=controls>
    ),  # </td>
    
    
    #-- The Left 2 Panel --#
    tags$td(class='controls_td controls_color',
            tags$div(class='controls',
                     do.call(tabsetPanel,
                             c(list(id='custom_unit_tabset_panel', type='pills'),
                               lapply(1:MAX.N.SUBPOPULATIONS, create.custom.intervention.unit.selector)
                               ))
            ) #</div>
    ), #</td>

    #-- The Right Panel --#
    tags$td(class='controls_td controls_color',
            rowspan=2,
            create.plot.control.panel('custom')
    )
    
), #</tr>

##-- CTA TEXT --##
tags$tr(
    
    #-- Left panel text --#
    tags$td(class='cta_text_td cta_background_color',
            HTML("This will take 2-5 minutes<BR>
                  <input type='checkbox' id='chime_run_custom' name='chime_run_custom' style='float: left'>
                  <label for='chime_run_custom'>&nbsp;Play a chime when done</label>")
    ),
    
), #</tr>

##-- CTA BUTTONS --##
tags$tr(
    
    #-- Left 2 panel button --#
    tags$td(class='cta_td cta_color',
            actionButton(class='cta cta_color', inputId='run_custom', label='Simulate Intervention')
            ),
    
    #-- Right panel button --#
    tags$td(class='cta_td cta_color',
            actionButton(class='cta cta_color', inputId='redraw_custom', label='Adjust Projections')
            )
    
) #</tr>

)) #</tbody></table>
