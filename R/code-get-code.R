#' Retrieve independent R code as text
#'
#' @param server the server function to pull code from
#' @param ... outputs to shake tree upon (or keep all code if no outputs are
#'   listed)
#' @param envir the environment in which to evaluate server arguments
#'
#' @return a string of code
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   selectInput('x', 'x axis', choices = names(mtcars)),
#'   selectInput('y', 'y axis', choices = names(mtcars)),
#'   plotOutput('plot'),
#'   verbatimTextOutput('code')
#' )
#' 
#' srv <- function(input, output, session) {
#'   output$plot <- renderPlot({
#'     plot(x = mtcars[[input$x]],
#'          y = mtcars[[input$y]])
#'   })
#'   output$code <- renderText({
#'     get_code(srv, 'plot')
#'   })
#' }
#' 
#' shinyApp(ui, srv)
#' }
#'
#' @export
get_code <- function(server, ..., envir = parent.frame()) {
  static_code <- generate_static_code(server = server, ..., envir = envir)
  paste(top_level_call_list(static_code), collapse = '\n')
}