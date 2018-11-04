## Example Adapted From:
## https://shiny.rstudio.com/gallery/module-example.html

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

## Dummy module - Unique Values in First Column

uniquesTableUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(12, verbatimTextOutput(ns("table")))
  )
}

uniquesTable <- function(input, output, session, data, col) {
  
  filteredData <- reactive({
    data() %>% dplyr::filter(selected_) %>% .[[col()]]
  })
  
  output$table <- renderPrint ({
    table(filteredData())
  })
  
  uniques <- reactive({ unique(filteredData()) })
  
  return(uniques)
}

## Linked scatter module

linkedScatterUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(6, plotOutput(ns("plot1"), brush = ns("brush"))),
      column(6, plotOutput(ns("plot2"), brush = ns("brush")))
    ),
    fluidRow(
      column(12, uniquesTableUI(ns("uniques_table")))
    ),
    fluidRow(
      column(12, verbatimTextOutput(ns("uniques")))
    )
  )
}

linkedScatter <- function(input, output, session, data, left, right) {
  u <- callModule(uniquesTable, "uniques_table", data = dataWithSelection, 
    col = reactive(right()[[1]]))
  
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
  
  output$uniques <- renderPrint({
    paste("Unique Values:", paste(as.character(u()), collapse = ", "))
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
  df <- callModule(linkedScatter, "scatters", reactive(my_data),
    left = reactive(c("cty", "hwy")),
    right = reactive(c("drv", "hwy")))
  
  output$summary <- renderText({
    sprintf("%d observation(s) selected", nrow(dplyr::filter(df(), selected_)))
  })
  
  output$code <- renderPrint(cat(get_code(server, "summary")))
}

shinyApp(ui, server)


# Can now access nested module outputs via
# 
# #> scatters %>% 
# #>   module("uniques_table") %>% 
# #>   get_output("table")