#' Helper for adding code-specific action button
#' 
#' @inheritParams shiny::actionButton
#' 
#' @examples
#' \dontrun{
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   scriptglossJS(),
#'   selectInput('x', 'x axis', choices = names(mtcars)),
#'   selectInput('y', 'y axis', choices = names(mtcars)),
#'   showCodeButton("show_code"),
#'   plotOutput('plot')
#' )
#' 
#' srv <- function(input, output, session) {
#'   output$plot <- renderPlot({
#'     plot(x = mtcars[[input$x]], y = mtcars[[input$y]])
#'   })
#'   observeEvent(input$show_code, show_code_modal(srv, "plot"))
#' }
#' 
#' shinyApp(ui, srv)
#' }
#' 
#' @importFrom shiny actionButton icon
#' @export
#' 
showCodeButton <- function(inputId, label = "Show R Code", 
  icon = shiny::icon("code"), ...) {
  
  check_shiny()
  shiny::actionButton(inputId, label, icon, ...)
}



#' Display independent code in modal dialog
#'
#' @description uses javascript libraries, PrismJS and ClipboardJS to highlight
#'   code and handle copying of code to clipboard respectively. Dependencies
#'   must be loaded in the module header. Helper functions
#'   \code{\link{prismDependencies}} and \code{\link{clipboardjsDependencies}}
#'   included to streamline loading from content distribution network.
#'
#' @param server The server function handle for construction of code
#' @param ... Any outputs for which code should be constructed. If none are
#'   passed, all code is produced.
#' @param title Title of the code modal
#' @param id css id of the code modal
#' @param envir environment for collecting code artifacts
#' @param code code used to generate output can be passed directly as an
#'   alternative to the server, ellipses and envir arguments
#'
#' @return a shiny modalDialog with copy to clipboard button and code necessary
#'   to generate outputs in server function
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   scriptglossJS(),
#'   selectInput('x', 'x axis', choices = names(mtcars)),
#'   selectInput('y', 'y axis', choices = names(mtcars)),
#'   actionButton("show_code", "Show R Code", icon("code")),
#'   plotOutput('plot')
#' )
#' 
#' srv <- function(input, output, session) {
#'   internal_data <- mtcars
#'   output$plot <- renderPlot({
#'     plot(
#'       x = internal_data[[input$x]],
#'       y = internal_data[[input$y]],
#'       xlab = input$x,
#'       ylab = input$y)
#'   })
#'   observeEvent(input$show_code, show_code_modal(srv, 'plot'))
#' }
#' 
#' shinyApp(ui, srv)
#' }
#'
#' @importFrom shiny showModal modalDialog tags tagList modalButton
#'
#' @export
show_code_modal <- function(server, ..., title = 'R Code',
  id = 'modal_clipboardjs_btn', envir = parent.frame(),
  code = get_code(server, ..., call_outputs = TRUE, envir = envir)) {
  
  check_shiny()
  shiny::showModal(shiny::modalDialog(
    pre_code(
      code, 
      style = "height: 60vh; overflow: auto !important;"),
    shiny::tags$script("Prism.highlightAll();"),
    footer = shiny::tagList(
      clipCodeButton(
        id = "modal_clipboardjs_btn", 
        text = code, 
        "Copy to Clipboard", 
        modal = TRUE),
      shiny::modalButton("Dismiss")
    ),
    title = title,
    easyClose = TRUE,
    size = 'l'
  ))
}



#' Create a <pre><code>...</code></pre> block
#'
#' @param text text to include in the code block
#' @param language the language to use for syntax highlighting. must be a valid
#'   prismJS language.
#' @param ... additional html attributes to use within code block.
#'
#' @return a html <pre><code>...</code></pre> block
#'
pre_code <- function(text, language = 'r', ...) {
  # clean up gaps in pre tag which cause formatting issues
  shiny::HTML(gsub('>\\s*<', '><', 
    shiny::tags$pre(shiny::tags$code(
      gsub('^\\s*|\\s*$', '', text),
      class = sprintf("language-%s", language),
      ...)
    )
  ))
}