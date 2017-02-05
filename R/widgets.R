example.wellRadioButtons = function() {
  app = eventsApp()
  app$ui = bootstrapPage(
    wellRadioButtons("rb",NULL,1:4),
    wellCheckboxGroupInput("cb",NULL,1:3)
  )
  viewApp(app)
}

wellCheckboxGroupInput = function (inputId, label, choices, selected = NULL, inline = FALSE,
    width = "100%")
{
    selected <- shiny:::restoreInput(id = inputId, default = selected)
    choices <- shiny:::choicesWithNames(choices)
    if (!is.null(selected))
        selected <- shiny:::validateSelected(selected, choices, inputId)
    options <- generateWellOptions(inputId, choices, selected, inline)
    divClass <- "form-group shiny-input-checkboxgroup shiny-input-container"
    tags$div(id = inputId, style = if (!is.null(width))
        paste0("width: ", width, ";"), class = divClass,
        shiny:::controlLabel(inputId, label), options)
}

wellRadioButtons = function (inputId, label=NULL, choices, selected = NA, width = "100%",...)
{
  restore.point("wellRadioButtons")

  choices <- shiny:::choicesWithNames(choices)
  if (is.null(selected)) selected = choices[[1]]
  if (length(selected) > 1)
    stop("The 'selected' argument must be of length 1")
  options <- generateWellOptions(inputId, choices, selected,type = "radio")
  divClass <- "form-group shiny-input-radiogroup shiny-input-container"
  tags$div(id = inputId, style = if (!is.null(width))
    paste0("width: ", validateCssUnit(width), ";"), class = divClass,
    if (!is.null(label)) shiny:::controlLabel(inputId, label), options)
}

generateWellOptions = function (inputId, choices, selected, inline=FALSE, type = "checkbox")
{
  options <- mapply(choices, names(choices), FUN = function(value,name) {
      inputTag <- tags$input(type = type, name = inputId, value = value, style ="padding-x: 6px;")
      if (value %in% selected)
          inputTag$attribs$checked <- "checked"
      tags$div(class = type, style="width: 100%",
        tags$label(inputTag,style=
"width: 100%;
 min-height: 16px;
 padding: 10px;
 padding-left: 25px;
 margin-left: 4px;
 margin-right: 4px;
 margin-bottom: 3px;
 background-color:#f5f5f5;
 border:1px solid #e3e3e3;
 border-radius: 4px;
 -webkit-box-shadow:inset 0 1px 1px rgba(0,0,0,0.05);
 box-shadow:inset 0 1px 1px rgba(0,0,0,0.05)", tags$span(name)))
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  div(class = "shiny-options-group", options)
}
