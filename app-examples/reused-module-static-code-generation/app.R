try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE) # R Studio
try(setwd(dirname(dirname(parent.frame(2)$ofile))), silent = TRUE) # running as a script
message(sprintf('Working directory changed to "%s"', getwd()))

## Global declarations

library(scriptgloss)
library(shiny)
library(dplyr)
library(ggplot2)

my_data <- ggplot2::mpg
my_data2 <- iris

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
  linkedScatterUI("scatters2"),
  textOutput("summary2"),
  verbatimTextOutput("code")
)

server <- function(input, output, session) {
  df <- callModule(linkedScatter, "scatters", reactive(my_data),
    left = reactive(c("cty", "hwy")),
    right = reactive(c("drv", "hwy")))
  
  df2 <- callModule(linkedScatter, "scatters2", reactive(my_data2),
    left = reactive(c("Sepal.Length", "Sepal.Width")),
    right = reactive(c("Species", "Sepal.Width")))

  output$summary <- renderText({
    sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
  })
  
  output$summary2 <- renderText({
    sprintf("%d observation(s) selected", nrow(dplyr::filter(df2(), selected_)))
  })
  
  output$code <- renderPrint(cat(get_code(server)))
}

shinyApp(ui, server)

