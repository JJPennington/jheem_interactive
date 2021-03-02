PRERUN.CONTENT = tags$table(class='display_table fill_page1', tags$tbody(class='display_tbody',
    
##-- HEADERS AND DISPLAY --##  
tags$tr(
    #-- Left Header --#
    tags$td(id='left_controls_prerun_header',
            class='controls_header_td controls_narrow header_color collapsible',
            tags$div(class='controls_narrow', "Select Intervention")
            ),
    
    #-- The Main Panel --#
    tags$td(class='display_td content_color', id='display_prerun_td',
            rowspan=4,
            tags$div(class='display',
                     create.display.panel('prerun')
            ),
                     
            #-- ACCORDION BUTTONS --#
            make.accordion.button('prerun_collapse_left', 
                                  left.offset ='-10px',
                                  direction='left',
                                  hide.ids=c('prerun_collapse_left'),
                                  show.ids='prerun_expand_left',
                                  remove.class.ids=c('left_controls_prerun','left_prerun_cta','left_prerun_cta_text','left_controls_prerun_header'),
                                  add.class.ids=c('left_controls_prerun','left_prerun_cta','left_prerun_cta_text','left_controls_prerun_header'),
                                  remove.classes='controls_narrow',
                                  add.classes='collapsed',
                                  shiny.ids='left_width_prerun',
                                  shiny.values=0,
                                  visible=T
            ),
            bsTooltip('prerun_collapse_left', 'Hide Intervention Selection', placement='right'),
            
            make.accordion.button('prerun_expand_left', 
                                  left.offset='0px',
                                  direction='right',
                                  show.ids=c('prerun_collapse_left'),
                                  remove.class.ids=c('left_controls_prerun','left_prerun_cta','left_prerun_cta_text','left_controls_prerun_header'),
                                  add.class.ids=c('left_controls_prerun','left_prerun_cta','left_prerun_cta_text','left_controls_prerun_header'),
                                  remove.classes='collapsed',
                                  add.classes='controls_narrow',
                                  shiny.ids='left_width_prerun',
                                  shiny.values=LEFT.PANEL.SIZE['prerun'],
                                  visible=F
            ),  
            make.popover('prerun_expand_left', 'Show Intervention Selection',
                         'Click for controls to select an intervention.',
                         placement='right'),
            
            
            make.accordion.button('prerun_collapse_right', 
                                  right.offset ='-10px',
                                  direction='right',
                                  show.ids=c('prerun_expand_right','custom_expand_right'),
                                  hide.ids=c('prerun_collapse_right','custom_collapse_right'),
                                  remove.class.ids=c('right_controls_prerun','right_prerun_cta','right_controls_prerun_header',
                                                     'right_controls_custom','right_custom_cta','right_controls_custom_header'),
                                  add.class.ids=c('right_controls_prerun','right_prerun_cta','right_controls_prerun_header',
                                                  'right_controls_custom','right_custom_cta','right_controls_custom_header'),
                                  remove.classes='controls_narrow',
                                  add.classes='collapsed',
                                  shiny.ids='right_width_prerun',
                                  shiny.values=0,
                                  visible=F
            ),
            bsTooltip('prerun_collapse_right', 'Hide Figure Settings', placement='left'),
            
            make.accordion.button('prerun_expand_right', 
                                  right.offset='0px',
                                  direction='left',
                                  show.ids=c('prerun_collapse_right','custom_collapse_right'),
                                  hide.ids=c('prerun_expand_right','custom_expand_right'),
                                  remove.class.ids=c('right_controls_prerun','right_prerun_cta','right_controls_prerun_header',
                                                     'right_controls_custom','right_custom_cta','right_controls_custom_header'),
                                  add.class.ids=c('right_controls_prerun','right_prerun_cta','right_controls_prerun_header',
                                                  'right_controls_custom','right_custom_cta','right_controls_custom_header'),
                                  remove.classes='collapsed',
                                  add.classes='controls_narrow',
                                  shiny.ids='right_width_prerun',
                                  shiny.values=RIGHT.PANEL.SIZE['prerun'],
                                  visible=T
            ),  
            make.popover('prerun_expand_right', 'Show Figure Settings',
                         'Click for controls to adjust what is plotted in the figures.',
                         placement='left')
                     
    ),
    
    #-- Right Header --#
    tags$td(id='right_controls_prerun_header',
            class='controls_header_td header_color collapsible collapsed',
            tags$div(class='controls_narrow', "Figure Settings"))
), #</tr>

##-- CONTROL PANELS --##
tags$tr(
    
    #-- The Left Panel --#
    tags$td(id='left_controls_prerun',
            class='controls_td controls_narrow controls_color collapsible',
            tags$div(class='controls controls_narrow',
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
    tags$td(id='right_controls_prerun',
            class='controls_td controls_color collapsible collapsed',
            rowspan=2,
            create.plot.control.panel('prerun')
    )
    
), #</tr>

##-- CTA TEXT --##
tags$tr(
    
    #-- Left panel text --#
    tags$td(id='left_prerun_cta_text',
            class='cta_text_td controls_narrow cta_background_color collapsible',
            tags$div(class='controls_narrow',
                tags$div(class='cta_text',
                    HTML("This will take 10-30 seconds<BR>
                          <input type='checkbox' id='chime_run_prerun' name='chime_run_prerun' style='float: left'>
                          <label for='chime_run_prerun'>&nbsp;Play a chime when done</label>")
                )
            )
    ),
    
), #</tr>

##-- CTA BUTTONS --##
tags$tr(
    
    #-- Left panel button --#
    tags$td(id='left_prerun_cta',
            class='cta_td controls_narrow cta_background_color collapsible',
            tags$div(class='controls_narrow', 
                     actionButton(class='cta cta_color', inputId='run_prerun', label='Generate Projections'))
            ),
    
    #-- Right panel button --#
    tags$td(id='right_prerun_cta',
            class='cta_td cta_background_color collapsible collapsed',
            tags$div(class='controls_narrow', 
                     actionButton(class='cta cta_color', inputId='redraw_prerun', label='Adjust Projections'))
            )
    
) #</tr>

)) #</tbody></table>