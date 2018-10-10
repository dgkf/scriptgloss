## Shiny Example (app.R contents)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE) # R Studio
try(setwd(dirname(dirname(parent.frame(2)$ofile))), silent = TRUE) # running as a script
message(sprintf('Working directory changed to "%s"', getwd()))

library(scriptgloss)
library(shiny)

ui <- fluidPage(
  selectInput('x', 'x axis', choices = names(mtcars)),
  selectInput('y', 'y axis', choices = names(mtcars)),
  downloadButton("export", "Download Report"),
  plotOutput('plot'),
  verbatimTextOutput("report_txt")
)

srv <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(x = mtcars[[input$x]],
         y = mtcars[[input$y]])
  })
  output$export <- report_download_handler('report.Rmd', srv)
  output$report_txt <- renderPrint({
    cat(report_from_template("report.Rmd", srv))
  })
}

shinyApp(ui, srv)