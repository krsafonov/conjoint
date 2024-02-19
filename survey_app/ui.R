library(shiny)

href <- 'https://krsafonov.shinyapps.io/post_survey_app/?user_id='


jscode <- paste0(
  sprintf("Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = '%s'+message;});", href),
  "Shiny.addCustomMessageHandler('activate_tooltips', function(x) {"
  ,"$('[data-toggle=\"tooltip\"]').tooltip({"
  ,"placement: 'right'"
  ,"});})")
                

fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "hover.css"),
    tags$script(jscode),
  ),
  # Put setnr on screen
  column(8, align = 'center', textOutput("set.nr")),
  # Put design on screen
  column(8, align = 'center', tableOutput("choice.set")),
  # Put answer options on screen
  column(8, align = 'center', uiOutput('buttons')), 
  # put introtext on screen
  column(8, align = 'center', htmlOutput('intro')),
  # Put action button on screen
  column(8, align = "center", actionButton("OK", "OK")),
  # put end text on screen
  column(8, align = 'center', textOutput('end'))
)