ui <- fluidPage(
  selectInput("a", NULL, names(mtcars)),
  selectInput("b", NULL, names(mtcars)),
  verbatimTextOutput("test"),
  verbatimTextOutput("code"))

srv <- function(input, output, session) {
  output$test <- renderPrint({
    reactiveValuesToList(input)
  })
  
  output$code <- renderPrint({
    scriptgloss:::append_declaration(quote(print("Here")),
      input = input,
      after = 0)
  })
}

shinyApp(ui, srv)