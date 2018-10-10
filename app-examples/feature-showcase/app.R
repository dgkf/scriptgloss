## Shiny Example (app.R contents)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE) # R Studio
try(setwd(dirname(dirname(parent.frame(2)$ofile))), silent = TRUE) # running as a script
message(sprintf('Working directory changed to "%s"', getwd()))

library(scriptgloss)
library(shiny)
library(ggplot2)


my_data <- mtcars


ui <- fluidPage(
  # javascript for syntax highlighting (prismJS) and clipbboard (clipboardJS)
  scriptglossJS(),
  
  # the usual plot input suspects
  selectInput('x', 'x axis', choices = names(mtcars)),
  selectInput('y', 'y axis', choices = names(mtcars)),
  selectInput('outputs', 'outputs',
    choices = c('plot1', 'plot2', 'code', 'report_txt'),
    multiple = TRUE),
  plotOutput('plot1'),
  plotOutput('plot2'),
  
  # code generation buttons
  downloadButton("export", "Download Report"),
  showCodeButton("show_code", "Show Plot Code", style = "display:inline-block"),
  uiOutput("copy_code_btn", style = "display:inline-block"),

  # introspection of code generation and reports
  headerPanel("Static Code:"),
  verbatimTextOutput('code'),
  headerPanel("R Markdown Report:"),
  verbatimTextOutput("report_txt")
)



srv <- function(input, output, session) {
  # code generation button callbacks
  output$export <- report_download_handler('report.Rmd', srv, 
      format = "html_document")
  output$copy_code_btn <- renderUI(clipCodeButton(srv))
  observeEvent(input$show_code, show_code_modal(srv, "plot1"))
  
  # plot outputs
  output$plot1 <- renderPlot({
    plot(
      x = my_data[[input$x]], 
      y = my_data[[input$y]], 
      xlab = input$x, 
      ylab = input$y)
  })
  
  output$plot2 <- renderPlot({
    ggplot(my_data) + 
      aes_string(x = input$x) + 
      geom_density()
  })

  # static code introspection
  output$code <- renderPrint(cat(get_code(srv, dots = input$outputs)))
  output$report_txt <- renderPrint(cat(report_from_template("report.Rmd", srv)))
}



shinyApp(ui, srv)