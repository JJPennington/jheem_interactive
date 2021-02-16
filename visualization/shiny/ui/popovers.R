
##---------------##
##-- TOOL-TIPS --##
##---------------##

PRERUN.POPOVER.TITLE = "Explore Pre-defined Interventions Quickly"
PRERUN.POPOVER = "This tab allows you to try out interventions that we have already simulated. You can get results within a few seconds."

CUSTOM.POPOVER.TITLE = "Define and Simulate Your Own Interventions"
CUSTOM.POPOVER = "This tab allows you to define any intervention you want. It will take several minutes to simulate these interventions."


# NB: these popover depends on a javascript hack to set the id of the title text, in setup_tooltips.js
make.tab.popover <- function(id,
                             title,
                             content)
{
    bsPopover(id, 
              title=paste0("<b>", title, "</b>"),
                  #title,#HTML(paste0("<a class='tab_popover_title'>", title, "</a>")),
              content=content,#HTML(paste0("<a class='tab_popover_content'>", content, "</a>")),
              trigger = "hover", placement='bottom',
              options=list(container="body", html=T))
}

make.popover <- function(id,
                         title,
                         content,
                         placement)
{
    bsPopover(id, 
              title=paste0("<b>", title, "</b>"),
              #title,#HTML(paste0("<a class='tab_popover_title'>", title, "</a>")),
              content=content,#HTML(paste0("<a class='tab_popover_content'>", content, "</a>")),
              trigger = "hover", placement=placement,
              options=list(container="body", html=T))
}