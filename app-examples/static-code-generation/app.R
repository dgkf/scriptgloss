## Shiny Example (app.R contents)
try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE) # R Studio
try(setwd(dirname(dirname(parent.frame(2)$ofile))), silent = TRUE) # running as a script
message(sprintf('Working directory changed to "%s"', getwd()))

library(datasets)
library(shiny)

my_data <- datasets::mtcars

# additional code before shinyApp call will be captured if in app.R
ui <- fluidPage(
  selectInput('x', 'x axis', choices = names(my_data)),
  selectInput('y', 'y axis', choices = names(my_data)),
  plotOutput('plot'),
  verbatimTextOutput('code')
)

srv <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(x = my_data[[input$x]],
         y = my_data[[input$y]])
  })
  output$code <- renderPrint({
    cat(get_code(srv, "plot"))
    # alternatively, to pull all outputs:
    # cat(get_code(srv))
  })
}

shinyApp(ui, srv)