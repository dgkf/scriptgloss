ui <- fluidPage(
  h1("reactiveVal"),
  actionButton("increment_rv", "Increment reactiveVal"),
  verbatimTextOutput("reactiveVal"),
  verbatimTextOutput("reactiveVal_code"))

srv <- function(input, output, session) {
  rv <- reactiveVal(0)
  
  observeEvent(input$increment_rv, rv(rv() + 1))
  
  output$reactiveVal <- renderPrint({
    rv()
  })
  
  output$reactiveVal_code <- renderPrint({
    cat(scriptgloss:::get_code(srv, "reactiveVal"))
  })
}

shinyApp(ui, srv)