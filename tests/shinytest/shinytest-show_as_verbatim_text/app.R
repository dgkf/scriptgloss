choices = list(
  list = list(1, 2, 3, 4, 5),
  mtcars = mtcars,
  character_vector = c("lorem", "ipsum", "other", "text"))

ui <- shiny::fluidPage(
 shiny::selectInput("in_item", 
    NULL, 
    choices = names(choices)),
  shiny::verbatimTextOutput("out_text")
)

srv <- function(input, output, session) {
  output$out_text <- shiny::renderPrint({
    print(choices[[input$in_item]])
  })
}

shiny::shinyApp(ui, srv)