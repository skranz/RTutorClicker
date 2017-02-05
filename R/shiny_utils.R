# smallButton <- function (inputId, label, icon = NULL, width = NULL,class=paste0("btn btn-default action-button ",size.class," ", extra.class), size.class="btn-xs",extra.class="",style=NULL, extra.style=NULL, ...)
# {
#   if (is.null(style)) {
#     style= paste0(if (!is.null(width)) paste0("width: ", shiny:::validateCssUnit(width), ";"),"")
#   }
#   if (!is.null(extra.style))
#     style = paste0(extra.style," ", style)
#   value <- restoreInput(id = inputId, default = NULL)
#   tags$button(id = inputId,  style=style , type = "button", class = class, `data-val` = value,list(shiny:::validateIcon(icon), label), ...)
# }

slimCollapsePanel = function (title, ..., value = title, bsStyle = NULL, heading.style="padding-top: 3px; padding-bottom: 3px;")
{
    content <- list(...)
    id <- paste0("cpanel", sprintf("%07i", as.integer(stats::runif(1,
        1, 1e+06))))
    if (is.null(value)) {
        value = title
    }
    if (is.null(bsStyle)) {
        bsStyle = "default"
    }
    bsTag <- shiny::tags$div(class = paste0("panel panel-", bsStyle),
        value = value, shiny::tags$div(class = "panel-heading", style=heading.style,role = "tab", id = paste0("heading_", id), shiny::tags$h4(class = "panel-title",
                shiny::tags$a(`data-toggle` = "collapse", href = paste0("#",
                  id), title))), shiny::tags$div(id = id, class = "panel-collapse collapse",
            role = "tabpanel", shiny::tags$div(class = "panel-body",
                content)))
    #htmltools::attachDependencies(bsTag, shinyBSDep)
}
