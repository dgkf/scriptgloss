ui <- fluidPage(
  h1("reactiveValues"),
  actionButton("increment_rvs", "Increment reactiveValues"),
  verbatimTextOutput("reactiveValues"),
  verbatimTextOutput("reactiveValues_code"))

srv <- function(input, output, session) {
  rvs <- reactiveValues(a = 1, b = 'a', c = FALSE)
  
  observeEvent(input$increment_rvs, {
    rvs$a <- rvs$a + 1
    rvs$b <- letters[1:rvs$a]
    rvs$c <- rvs$a %% 2 == 0
  })
  
  output$reactiveValues <- renderPrint({
    reactiveValuesToList(rvs)
  })
  
  output$reactiveValues_code <- renderPrint({
    cat(scriptgloss:::get_code(srv, "reactiveValues"))
  })
}

shinyApp(ui, srv)