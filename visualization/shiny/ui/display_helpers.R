
##------------------------------##
##-- CREATE THE DISPLAY PANEL --##
##------------------------------##

create.display.panel <- function(suffix)
{
    tags$table(class='display_panel_table',
        tags$tr(class='display_panel_main_tr', tags$td(
            tabsetPanel(
                id=paste0('nav_', suffix),
                tabPanel(
                    title="Figure",
                    uiOutput(outputId = paste0('figure_', suffix))
                ),
                tabPanel(
                    title="Table",
                    uiOutput(outputId = paste0('table_', suffix))
                ),
                navbarMenu(
                    title="Share",
                    menuName = paste0("share_", suffix),
                    icon = icon('arrow-up-square',lib='glyphicon'),
                    tabPanel(
                        title=actionLink(paste0('download_figure_', suffix), 'Download Figure'),
                    ),
                    tabPanel(
                        title=actionLink(paste0('download_table_', suffix), 'Download Table'),
                    ),
                    tabPanel(
                        title=actionLink(paste0('share_link_', suffix), 'Share Link'),
                    )
                )  # </navbarMenu>
            ),  # </tabsetPanel>
        )), # </td></tr>
        
        tags$tr(class='display_panel_intervention_tr', tags$td(
            create.projected.intervention.panel(suffix)
        )), #</td></tr>
    ) #</table>
}

create.projected.intervention.panel <- function(suffix)
{
    #I have hacked CSS (with file box_colors.css) to use custom color for 'info' status boxes
    box(title='Details of Projected Intervention',
        width=12,
        collapsible = T,
        status='info',
        solidHeader = T,
        uiOutput(outputId = paste0('selected_intervention_', suffix)))    
}

##-----------------------------##
##-- MANIPULATE THE CONTENTS --##
##-----------------------------##

set.display <- function(input, output, suffix, plot.and.table)
{
    set.plot(output, suffix, plot.and.table$plot)
    set.table(output, suffix, plot.and.table$change.df)
    set.intervention.panel(output, suffix, plot.and.table$intervention)
}

clear.display <- function(input, output, suffix)
{
    clear.plot(output, input, suffix)
    clear.table(output, input, suffix)
    clear.intervention.panel(output, input, suffix)
}


set.plot <- function(output,
                     suffix,
                     plot)
{
    holder.id = paste0('figure_', suffix)
    plot.id = paste0('plot_', suffix)
    if (suffix=='custom')
        css.class = 'plot_holder display_narrow'
    else
        css.class = 'plot_holder display_wide'
    
    output[[holder.id]] = renderUI(tags$div(class=css.class,
                                           withSpinner(plotlyOutput(outputId = plot.id))
    ))
    
    output[[plot.id]] = renderPlotly(plot)
}

set.table <- function(output,
                      suffix,
                      tab)
{
    table.id = paste0('table_', suffix)
    dt.id = paste0('dt_', suffix)
    if (suffix=='custom')
        css.class = 'table_holder display_narrow'
    else
        css.class = 'table_holder display_wide'
    
    output[[table.id]] = renderUI(tags$div(class=css.class,
        withSpinner(DT::DTOutput(outputId = dt.id))
    ))
    
    pretty.table = make.pretty.change.data.frame(tab,
                                                 data.type.names=WEB.DATA.TYPE.NAMES)
    #data.table(pretty.table)
    output[[dt.id]] = DT::renderDT(pretty.table)
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
            tags$div(make.intervention.pretty.table(intervention))
         #   tags$div(HTML(get.intervention.html.description(intervention)))
        )
        
}

clear.plot <- function(output,
                       input,
                       suffix)
{
    figure.id = paste0('figure_', suffix)
    output[[figure.id]] = renderUI(
        tags$div(class='empty_message',
                 tags$div(style='height:10vh'),
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
                 tags$div(style='height:10vh'),
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
            tags$div(make.intervention.pretty.table(intervention)))
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
    nav.id = paste0('nav_', suffix)
    target.value = paste0("share_", suffix)
    if (enabled)
        showTab(nav.id, target.value, select=F)
    else
        hideTab(nav.id, target.value)
       
    #OLD
#    ids = paste0(c('download_figure_', 'download_table_', 'share_link_'),
#                 suffix)
    
#    for (id in ids)
#    {
#        if (enabled)
#        {
#            shinyjs::enable(id)
#        }
#        else
#        {
#            shinyjs::disable(id)
#        }
#    }
}

##-- LOWER LEVEL HELPERS --##

make.intervention.pretty.table <- function(int)
{
#    int = lump.idu.for.intervention(int)
    raw = get.intervention.description.table(int, include.start.text = NULL,
                                             testing.descriptor='',
                                             prep.descriptor='uptake',
                                             suppression.descriptor='',
                                             empty.value = '-')
    target.population.names = attr(raw, 'target.population.names')
#    target.population.names = lump.idu.in.name(target.population.names)

    # Rorder to group by lumped idu
    lumped.tpop.names = sapply(lapply(attr(raw, 'target.populations'), lump.idu.in.target.population), target.population.name)
    lumped.tpop.first.index = sapply(lumped.tpop.names, function(name){
        (1:length(lumped.tpop.names))[lumped.tpop.names==name][1]
    })
    o = order(lumped.tpop.first.index)
    
    target.population.names = target.population.names[o]
    raw.dim = dim(raw)
    raw = raw[o,]
    dim(raw) = raw.dim
    
    # Make the table
    header.tds = c(list(tags$th()),
                   lapply(attr(raw, 'unit.types'), tags$th))
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
}