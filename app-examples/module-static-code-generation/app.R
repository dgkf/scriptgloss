## Example Adapted From:
## https://shiny.rstudio.com/gallery/module-example.html


## callModule Steps (Method #1)
## 
##   1. convert to a function
##      ```
##      linkedScatter <- function(input, data, left, right) {
##          dataWithSelection <- reactive({
##            brushedPoints(data(), input$brush, allRows = TRUE)
##          })
##        
##          output$plot1 <- renderPlot({
##            scatterPlot(dataWithSelection(), left())
##          })
##        
##          output$plot2 <- renderPlot({
##            scatterPlot(dataWithSelection(), right())
##          })
##        
##          return(dataWithSelection)
##      }
##      ```
##   
##   2. shake function body for returned value expression dependencies and
##   convert reactive expressions to nullary functions.
##      ```
##      linkedScatter <- function(input, data, left, right) {
##          dataWithSelection <- function() {
##            brushedPoints(data(), input$brush, allRows = TRUE)
##          }
##        
##          return(dataWithSelection)
##      }
##      ```
##    
##   3. replace `callModule` calls with function calls. reactives get replaced
##   with nullary functions returning reactive expression. inputs get replaced
##   with reactiveValuesToList(input) (in module scope).
##      ```
##      df <- linkedScatter(list(brush = NULL), function() my_data, 
##        function() c("cty", "hwy"), function() c("drv", "hwy))
##      ```
##    
## Considerations:
##   - plot outputs internal to the module are unavailable for static code
##   generation
##

## callModule Steps (Method #2)
## 
##   1. extract entire module body and process into static code
##   ```
##   input <- << reactiveValuesToList(input) in module scope >>
##   output <- list()
##   data <- function() my_data
##   left <- function() c("cty", "hwy")
##   right <- function() c("drv", "hwy")
##   
##   dataWithSelection <- function() {
##     brushedPoints(data(), input$brush, allRows = TRUE)
##   }
##   
##   output$plot1 <- renderPlot({
##     scatterPlot(dataWithSelection(), left())
##   })
##   
##   output$plot2 <- renderPlot({
##     scatterPlot(dataWithSelection(), right())
##   })
##   
##   dataWithSelection()
##   ```
##   
##   2. "namespace" all locally scoped variables into module id list
##   ```
##   scatter <- list()
##   scatter$input <- << reactiveValuesToList(input) in module scope >>
##   scatter$output <- list()
##   scatter$data <- function() my_data
##   scatter$left <- function() c("cty", "hwy")
##   scatter$right <- function() c("drv", "hwy")
##   
##   scatter$dataWithSelection <- function() {
##     brushedPoints(data(), scatters$input$brush, allRows = TRUE)
##   }
##   
##   scatters$output$plot1 <- renderPlot({
##     scatterPlot(scatter$dataWithSelection(), scatter$left())
##   })
##   
##   scatters$output$plot2 <- renderPlot({
##     scatterPlot(scatter$dataWithSelection(), scatter$right())
##   })
##   
##   scatter$return <- scatter$dataWithSelection
##   ```
##   
##   
##   3. flatten module code into calling server body and replace `callModule`
##   statements with return value
##   ```
##   scatter <- structure(list(), class = c("scriptgloss-module", "list"))
##   scatter$input <- << reactiveValuesToList(input) in module scope >>
##   scatter$output <- list()
##   scatter$data <- function() my_data
##   scatter$left <- function() c("cty", "hwy")
##   scatter$right <- function() c("drv", "hwy")
##   
##   scatter$dataWithSelection <- function() {
##     brushedPoints(data(), scatters$input$brush, allRows = TRUE)
##   }
##   
##   scatters$output$plot1 <- renderPlot({
##     scatterPlot(scatter$dataWithSelection(), scatter$left())
##   })
##   
##   scatters$output$plot2 <- renderPlot({
##     scatterPlot(scatter$dataWithSelection(), scatter$right())
##   })
##   
##   scatter$return <- scatter$dataWithSelection
##   
##   df <- scatter$return
##   
##   output$summary <- function() {
##     sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
##   }
##   ```
##   
## Considerations
##   - Does creation of a minimal S3 class help to make the structure more
##   clear? Could potentially add a show method to make it more discoverable.
##   - Code gets very cluttered with large list structure
##   


try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE) # R Studio
try(setwd(dirname(dirname(parent.frame(2)$ofile))), silent = TRUE) # running as a script
message(sprintf('Working directory changed to "%s"', getwd()))

## Global declarations

library(scriptgloss)
library(shiny)
library(dplyr)
library(ggplot2)

my_data <- ggplot2::mpg

scatterPlot <- function(data, cols) {
  ggplot(data, aes_string(x = cols[1], y = cols[2])) +
    geom_point(aes(color = selected_)) +
    scale_color_manual(values = c("black", "#66D65C"), guide = FALSE)
}

## Linked scatter module

linkedScatterUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
    column(6, plotOutput(ns("plot2"), brush = ns("brush")))
  )
}

linkedScatter <- function(input, output, session, data, left, right) {
  # Yields the data frame with an additional column "selected_"
  # that indicates whether that observation is brushed
  dataWithSelection <- reactive({
    brushedPoints(data(), input$brush, allRows = TRUE)
  })

  output$plot1 <- renderPlot({
    scatterPlot(dataWithSelection(), left())
  })

  output$plot2 <- renderPlot({
    scatterPlot(dataWithSelection(), right())
  })

  return(dataWithSelection)
}

## Main shiny app server/ui

ui <- fixedPage(
  h2("Module example"),
  linkedScatterUI("scatters"),
  textOutput("summary"),
  verbatimTextOutput("code")
)

server <- function(input, output, session) {
  tmp <<- session
  
  df <- callModule(linkedScatter, "scatters", reactive(my_data),
    left = reactive(c("cty", "hwy")),
    right = reactive(c("drv", "hwy"))
  )

  output$summary <- renderText({
    sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
  })
  
  output$code <- renderPrint(cat(get_code(server, "summary")))
}

shinyApp(ui, server)

