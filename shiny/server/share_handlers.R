
get.default.download.filename <- function(plot.and.table,
                                          ext='')
{
    if (ext != '' && !grepl("^\\.", ext))
        ext = paste0(".", ext)
    
    if (length(plot.and.table$control.settings$facet.by)==0 && length(plot.and.table$control.settings$split.by)==0)
        by.suffix = ''
    else
        by.suffix = paste0(" by ",
                           paste0(unique(c(plot.and.table$control.settings$facet.by,
                                           plot.and.table$control.settings$split.by)), 
                                  collapse=', ')
        )
    
    paste0(get.location.short.name(plot.and.table$main.settings$location), " - ",
           paste0(WEB.DATA.TYPE.NAMES[plot.and.table$control.settings$data.types], collapse=", "),
           by.suffix,
           ext)
}

MIN.DOWNLOAD.FIGURE.DIM = 20
MAX.DOWNLOAD.FIGURE.DIM = 10000

show.download.plot.modal <- function(session, plot.and.table, input, suffix)
{
    figure.size = get.figure.size(plot.and.table,
                                  input=input, 
                                  suffix=suffix)
    
    modal.title = tags$div(
        style='font-weight: bold; text-align: center; font-size: 1.5em',
        'Download Figure'
    )
    
    modal.footer = tags$div(
        style='text-align: center',
        actionButton(inputId=paste0('do_download_figure_', suffix),
                     label=tags$div(
                         style='font-size: 1.5em;',
                         "Download")
                     ),
        modalButton(label=tags$div(
            style='font-size: 1.5em;',
            "Cancel")
        )
    )
    
    modal.content = tags$table(style='margin: 0 auto;',
        tags$tr(
            tags$td(style='vertical-align: top',
                numericInputIcon(inputId = paste0('download_figure_width_', suffix),
                                 label = tags$div(
                                         style='font-size: 1.25em',
                                         'Figure Width'),
                                 value = round(figure.size$width),
                                 min = MIN.DOWNLOAD.FIGURE.DIM,
                                 max = MAX.DOWNLOAD.FIGURE.DIM,
                                 icon=list(NULL,'px'),
                                 size='lg',
                                 width='150px',
                                 help_text = HTML(paste0("width must be between<BR>", MIN.DOWNLOAD.FIGURE.DIM,
                                                    " and ", format(MAX.DOWNLOAD.FIGURE.DIM, big.mark=','),
                                                    " pixels")))
            ),
            
            tags$td(style='width: 40px'),
            tags$td(style='vertical-align: top',
                numericInputIcon(inputId = paste0('download_figure_height_', suffix),
                                 label = tags$div(
                                     style='font-size: 1.25em',
                                     'Figure Height'),
                                 value = round(figure.size$height),
                                 min = MIN.DOWNLOAD.FIGURE.DIM,
                                 max = MAX.DOWNLOAD.FIGURE.DIM,
                                 icon=list(NULL,'px'),
                                 size='lg',
                                 width='150px',
                                 help_text = HTML(paste0("height must be between<BR>", MIN.DOWNLOAD.FIGURE.DIM,
                                                    " and ", format(MAX.DOWNLOAD.FIGURE.DIM, big.mark=','),
                                                    " pixels")))
            )
        )
    )
    
    modal = modalDialog(
        title=modal.title,
        modal.content,
        size='m',
        footer = modal.footer,
    )
    
    showModal(modal, session=session)
}


do.download.plot <- function(plot.and.table,
                             suffix,
                             width, height)
{
    width = min(MAX.DOWNLOAD.FIGURE.DIM,
                max(MIN.DOWNLOAD.FIGURE.DIM, width))
    height = min(MAX.DOWNLOAD.FIGURE.DIM,
                max(MIN.DOWNLOAD.FIGURE.DIM, height))
    
    
    js$download_plotly(id = get.plot.id(suffix),
                       width = width,
                       height = height,
                       filename = get.default.download.filename(plot.and.table,
                                                                ext=''))
}

# https://plotly.com/javascript/static-image-export/#saving-as-png

