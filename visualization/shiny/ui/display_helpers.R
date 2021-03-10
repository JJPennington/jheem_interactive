library(DT)

##------------------------------##
##-- CREATE THE DISPLAY PANEL --##
##------------------------------##

create.display.panel <- function(suffix)
{
    
    if (suffix=='custom')
        css.class = 'display_panel_table display_narrow_smaller'
    else
        css.class = 'display_panel_table display_wide_smaller'
    
    if (1==2)
    {
    tags$table(class=css.class,
               create.share.menu(suffix),
               
        tags$tr(class='display_panel_main_tr', tags$td(class='display_panel_main_td',
            tabsetPanel(
                id=paste0('nav_', suffix),
                tabPanel(
                    title="Figure",
                    uiOutput(outputId = paste0('figure_', suffix), class='fill_div')
                ),
                tabPanel(
                    title="Table",
                    uiOutput(outputId = paste0('table_', suffix), class='fill_div')
                )
            ),  # </tabsetPanel>
        )), # </td></tr>
        
        tags$tr(class='display_panel_intervention_tr', tags$td(class='intervention_panel_buffer',
            ' '
        )), #</td></tr>
        create.projected.intervention.panel(suffix)
    ) #</table>
    }
    
    tabsetPanel(
        id=paste0('nav_', suffix),
        tabPanel(
            title="Figure",
            uiOutput(outputId = paste0('figure_', suffix), class='fill_div')
        ),
        tabPanel(
            title="Table",
            uiOutput(outputId = paste0('table_', suffix), class='fill_div')
        )
    )  # </tabsetPanel>
}


create.share.menu <- function(suffix)
{
    share.icon = '<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-box-arrow-up" viewBox="0 0 16 16">
        <path fill-rule="evenodd" d="M3.5 6a.5.5 0 0 0-.5.5v8a.5.5 0 0 0 .5.5h9a.5.5 0 0 0 .5-.5v-8a.5.5 0 0 0-.5-.5h-2a.5.5 0 0 1 0-1h2A1.5 1.5 0 0 1 14 6.5v8a1.5 1.5 0 0 1-1.5 1.5h-9A1.5 1.5 0 0 1 2 14.5v-8A1.5 1.5 0 0 1 3.5 5h2a.5.5 0 0 1 0 1h-2z"/>
        <path fill-rule="evenodd" d="M7.646.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708L8.5 1.707V10.5a.5.5 0 0 1-1 0V1.707L5.354 3.854a.5.5 0 1 1-.708-.708l3-3z"/>
        </svg>'
    
    tags$div(class='share_container shaded_dropdown',
             dropdownButton(
                 label=HTML(paste0(share.icon, ' Share')),
                 circle=F,
                 icon=icon('box-arrow-up', lib='glyphicon'),
                 right=T,
            #    status = 'info',
                 inputId = paste0('share_menu_', suffix),
                 actionLink(inputId = paste0('download_figure_', suffix),
                            label = HTML("Download&nbsp;Figure")),
                 downloadLink(outputId = paste0('download_table_', suffix),
                            label = HTML("Download&nbsp;Table")),
                 actionLink(inputId = paste0('share_link_', suffix),
                            label = HTML("Share&nbsp;Link"))
             ))
}

create.projected.intervention.panel <- function(suffix)
{
    if (1==1)
    {
    #I have hacked CSS (with file box_colors.css) to use custom color for 'info' status boxes
    tags$div(class='intervention_panel_holder',
                      box(title='Details of Projected Intervention',
                          width=12,
                          collapsible = T,
                          collapsed = T,
                          status='info',
                          solidHeader = T,
                          uiOutput(outputId = paste0('selected_intervention_', suffix)))
             
    )
    }   
    else
    {
    tags$div(class='intervention_panel_holder',
             make.accordion.div(
               id='collapse_intervention_panel',
               class='intervention_panel_header header_color accordion_trigger',
               style='display: none',
               "Details of Projected Intervention",
               tags$div(style='float: right', icon('caret-down')),
               
               hide.ids = 'collapse_intervention_panel',
               show.ids = 'expand_intervention_panel',
               remove.class.ids = 'intervention_panel_body',
               remove.classes='expanded_vertical',
               add.classes='collapsed_vertical'
               
             ),
             make.accordion.div(
               id='expand_intervention_panel',
               class='intervention_panel_header header_color accordion_trigger',
               "Details of Projected Intervention",
               tags$div(style='float: right', icon('caret-up')),
               
               hide.ids = 'expand_intervention_panel',
               show.ids = 'collapse_intervention_panel',
               remove.class.ids = 'intervention_panel_body',
               remove.classes='collapsed_vertical',
               add.classes='expanded_vertical'
             ),
             tags$div(
               id = 'intervention_panel_body',
               class = 'collapsible_vertical collapsed_vertical',
               tags$div(
                 class='intervention_panel_body controls_color',
                 uiOutput(outputId = paste0('selected_intervention_', suffix))
               )
             )
    )
    }
}

##-----------------------------##
##-- MANIPULATE THE CONTENTS --##
##-----------------------------##

set.display <- function(input, output, suffix, plot.and.table)
{
    set.plot(input, output, suffix, plot.and.table)
    set.table(input, output, suffix, plot.and.table$change.df)
    set.intervention.panel(output, suffix, plot.and.table$intervention)
}

clear.display <- function(input, output, suffix)
{
    clear.plot(output, input, suffix)
    clear.table(output, input, suffix)
    clear.intervention.panel(output, input, suffix)
}

set.plot <- function(input,
                     output,
                     suffix,
                     plot.and.table)
{
    holder.id = paste0('figure_', suffix)
    plot.id = paste0('plot_', suffix)
    
    display.size = get.display.size(input, suffix)
    holder.height = display.size$height - DISPLAY_Y_CUSHION
    figure.size = get.figure.size(plot.and.table,
                                  input=input,
                                  suffix=suffix)    
    height = figure.size$height
    

    print(paste0('holder.height=', holder.height, ', figure height=', height))
    
    output[[holder.id]] = renderUI(tags$div(class='plot_holder',
                                            style=paste0('max-height: ', holder.height, 'px;'),                                            withSpinner(type=1,
                                               plotlyOutput(outputId = plot.id,
                                                                    height=paste0(height, 'px')))
    ))
    
    do.render.plot(input=input,
                   output=output,
                   suffix=suffix,
                   plot=plot.and.table)
}

do.render.plot = function(input,
                          output,
                          suffix,
                          plot.and.table)
{
    settings = calculate.optimal.nrows.and.label.size(plot.and.table, input, suffix)
    
    the.plot = do.plot.from.components(plot.and.table$plot, 
                                       nrows=settings$nrows,
                                       label.change.size = settings$label.size)
    the.plot = format.plotly.toolbar(the.plot)
    
    plot.id = get.plot.id(suffix)
    output[[plot.id]] = renderPlotly(the.plot)
}

get.plot.id <- function(suffix)
{
    paste0('plot_', suffix)
}

set.table <- function(input,
                      output,
                      suffix,
                      tab)
{
    table.id = paste0('table_', suffix)
    dt.id = paste0('dt_', suffix)
    
    display.size = get.display.size(input, suffix)
    width = display.size$width - DISPLAY_X_CUSHION
    height = display.size$height - DISPLAY_Y_CUSHION
    
    output[[table.id]] = renderUI(tags$div(class='table_holder',
                                          style=paste0('max-height: ', height, 'px; max-width: ', width, 'px;'),
        withSpinner(type=7,
        DT::dataTableOutput(outputId = dt.id)#,
#                            width=paste0(width, 'px'))
        )
    ))
    
    pretty.table = make.pretty.change.data.frame(tab, data.type.names=WEB.DATA.TYPE.NAMES)
    pretty.table = DT::datatable(pretty.table)#,
#                                 options=list(scrollX=T, scrollY=T))
   
    output[[dt.id]] = DT::renderDataTable(pretty.table)
}

set.intervention.panel <- function(output,
                                   suffix,
                                   intervention)
{
    panel.id = paste0('selected_intervention_', suffix)
    if (is.null(intervention))
        output[[panel.id]] = renderUI(
            tags$div("No intervention has been set")
            )
    else
        output[[panel.id]] = renderUI(
            tags$div(make.intervention.pretty.table(intervention,
                                                    use.default.tpop.names = suffix=='custom'))
        )
        
}

clear.plot <- function(output,
                       input,
                       suffix)
{
    figure.id = paste0('figure_', suffix)
    output[[figure.id]] = renderUI(
        tags$div(class='empty_message',
                 "Use the controls to the left to generate a figure for ",
                 msa.names(get.selected.location(input, suffix)))
    )
}

clear.table <- function(output,
                        input,
                        suffix)
{
    table.id = paste0('table_', suffix)
    output[[table.id]] = renderUI(
        tags$div(class='empty_message',
                 "Use the controls to the left to generate a table for ",
                 msa.names(get.selected.location(input, suffix)))
    )
}

clear.intervention.panel <- function(output,
                                      input,
                                      suffix)
{
    panel.id = paste0('selected_intervention_', suffix)
    output[[panel.id]] = renderUI(
        tags$div("No intervention has been set")
    )
    
    if (1==2) #for testing - so we don't have to make the projections before seeing what the table looks like
    {
        intervention = INTERVENTION.MANAGER.1.0$intervention[[48]]
        
        output[[panel.id]] = renderUI(
            tags$div(make.intervention.pretty.table(intervention, T)))
    }
}

set.run.button.enabled <- function(input,
                                       suffix,
                                       enabled)
{
    if (enabled)
    {
        shinyjs::enable(paste0('run_', suffix))
    }
    else
    {
        shinyjs::disable(paste0('run_', suffix))
    }
}


set.redraw.button.enabled <- function(input,
                                    suffix,
                                    enabled)
{
    if (enabled)
    {
        shinyjs::enable(paste0('redraw_', suffix))
    }
    else
    {
        shinyjs::disable(paste0('redraw_', suffix))
    }
}

set.share.enabled <- function(input,
                               suffix,
                               enabled)
{
    if (enabled)
        shinyjs::enable(paste0('share_menu_', suffix))
    else
        shinyjs::disable(paste0('share_menu_', suffix))
}

##----------##
##-- NCOL --##
##----------##

MIN.PANEL.WIDTH = 300
MIN.PANEL.HEIGHT = 280

get.figure.size <- function(plot.and.table, 
                            input, suffix,
                            ideal.w.h.ratio=1.5,
                            y.cushion=DISPLAY_Y_CUSHION+2*FIGURE.PADDING)
{
    display.size = get.display.size(input, suffix)
    
    settings = do.calculate.optimal.nrows(n.panels = get.num.panels.to.plot(plot.and.table$control.settings),
                                       display.width = display.size$width,
                                       display.height = display.size$height,
                                       ideal.w.h.ratio = ideal.w.h.ratio)

    list(width=display.size$width,
         height=max(display.size$height-y.cushion,
             settings$nrows * MIN.PANEL.HEIGHT))
    
}

calculate.optimal.nrows.and.label.size <- function(plot.and.table,
                                                   input, suffix,
                                                   ideal.w.h.ratio=1.5)
{
    display.size = get.display.size(input, suffix)
    
    settings = do.calculate.optimal.nrows(n.panels = get.num.panels.to.plot(plot.and.table$control.settings),
                                       display.width = display.size$width,
                                       display.height = display.size$height,
                                       ideal.w.h.ratio = ideal.w.h.ratio)
    
    settings$label.size=do.calculate.label.height(display.size$height, settings$nrows)
    
    settings
}

do.calculate.label.height <- function(height,nrows)
{
    height.per.panel = height / nrows
    
    #12 / log(700) * log(height.per.panel)
    
    max(5,
        height.per.panel^.25 * 14 / (700^.25))
}

do.calculate.optimal.nrows <- function(n.panels,
                                       display.width,
                                       display.height,
                                       ideal.w.h.ratio=1.5)
{
    possible.n.col = 1:n.panels
    possible.n.row = ceiling(n.panels / possible.n.col)
    
    possible.widths = display.width / possible.n.col
    possible.heights = display.height / possible.n.row
    
    possible.ratios = possible.widths/possible.heights
    
    ratio.diff = abs(possible.ratios - ideal.w.h.ratio)
    best.mask = ratio.diff == min(ratio.diff)
    
    best.fit.nrows = possible.n.row[best.mask][1]
    best.fit.ncols = ceiling(n.panels / best.fit.nrows)
    
    best.fit.panel.width = display.width / best.fit.ncols
    if (best.fit.panel.width < MIN.PANEL.WIDTH)
    {
        min.width.ncols = max(1, floor(display.width / MIN.PANEL.WIDTH))
        min.width.nrows = ceiling(n.panels / min.width.ncols)
        
        list(nrows=min.width.nrows,
             ncols=min.width.ncols)
    }
    else
    {
        list(nrows=best.fit.nrows,
             ncols=best.fit.ncols)
    }
}

get.num.panels.to.plot <- function(control.settings)
{
    selected.outcomes = control.settings$data.types
    n.selected.outcomes = length(selected.outcomes)
    
    facet.by = control.settings$facet.by
    if (is.null(facet.by))
        n.facet = 1
    else
        n.facet = sapply(facet.by, function(ff){
            length(DIMENSION.VALUES.2[[ff]]$values)
        })
    
    n.selected.outcomes * prod(n.facet)
}

##-------------------------##
##-- LOWER LEVEL HELPERS --##
##-------------------------##

make.intervention.pretty.table <- function(int, use.default.tpop.names)
{
#    tryCatch({
        raw = get.intervention.description.table(int, include.start.text = NULL,
                                                 testing.descriptor='',
                                                 prep.descriptor='uptake',
                                                 suppression.descriptor='',
                                                 empty.value = '-')
        target.populations = attr(raw, 'target.populations')
        unit.types = attr(raw, 'unit.types')
    #    target.population.names = lump.idu.in.name(target.population.names)
    
        if (use.default.tpop.names)
        {
            target.population.names = sapply(target.populations, default.target.population.name)
        }
        else
        {
            target.population.names = sapply(target.populations, target.population.name)
            
            # Reorder to group by lumped idu
            lumped.tpop.names = sapply(lapply(target.populations, lump.idu.in.target.population), target.population.name)
            lumped.tpop.first.index = sapply(lumped.tpop.names, function(name){
                (1:length(lumped.tpop.names))[lumped.tpop.names==name][1]
            })
            o = order(lumped.tpop.first.index)
            
            target.population.names = target.population.names[o]
            raw.dim = dim(raw)
            raw = raw[o,]
            dim(raw) = raw.dim
        }
        
        # Make the table
        header.tds = c(list(tags$th()),
                       lapply(unit.types, tags$th))
        names(header.tds) = NULL
        header.tr = do.call(tags$tr, header.tds)
        
        other.trs = lapply(1:length(target.population.names), function(i){
            tds = c(list(tags$td(target.population.names[i])),
                    lapply(raw[i,], tags$td))
            names(tds) = NULL
            do.call(tags$tr, tds)
        })
        
        all.trs = c(list(header.tr), other.trs)
        do.call(tags$table, list(all.trs, class='intervention_summary'))
  #  },
   # error = function(e){
    #    log.error(e)
     #   tags$div(class='error_message', 
      #                "There was an error generating a summary of the selected intervention. We apologize - the rest of the app will continue to function")
    #})
}
