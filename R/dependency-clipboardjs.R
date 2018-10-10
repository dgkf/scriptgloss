#' ClipboardJS source tags
#'
#' @return source tags for loading ClipboardJS from cdn
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' ui <- fluidPage(
#'   prismDependencies('r', 'coy'),
#'   clipboardjsDependencies(),
#'   selectInput('x', 'x axis', choices = names(mtcars)),
#'   selectInput('y', 'y axis', choices = names(mtcars)),
#'   uiOutput('copy_code_btn'),
#'   plotOutput('plot')
#' )
#' srv <- function(input, output, session) {
#'   output$plot <- renderPlot({
#'     plot(x = mtcars[[input$x]],
#'          y = mtcars[[input$y]])
#'   })
#'   output$copy_code_btn <- renderUI({
#'     clipCodeButton(id = "clipboard_btn", text = get_code(srv))
#'   })
#' }
#' shinyApp(ui, srv)
#' }
#'
#' @importFrom shiny tagList singleton tags
#' @export
clipboardjsDependencies <- function() {
  shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$script(
    src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/1.7.1/clipboard.min.js"
  )))) }
