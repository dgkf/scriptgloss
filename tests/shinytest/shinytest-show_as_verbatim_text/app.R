choices = list(
  list = list(1, 2, 3, 4, 5),
  mtcars = mtcars,
  character_vector = c("lorem", "ipsum", "other", "text"))

ui <- fluidPage(
 selectInput("in_item", 
    NULL, 
    choices = names(choices)),
  verbatimTextOutput("out_text")
)

srv <- function(input, output, session) {
  output$out_text <- renderPrint({
    print(choices[[input$in_item]])
  })
}

shinyApp(ui, srv)