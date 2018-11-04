#' Add button to copy text to clipboard
#'
#' @param server server function to derive code from
#' @param label a button label to be displayed with the button
#' @param icon an icon to be displayed with the button
#' @param modal whether the button is being shown in a modal dialog
#' @param id button css id
#' @param envir environment from which code should be collected
#' @param text the text to be copied
#'
#' @return a shiny tagList containing the button
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   scriptglossJS(),
#'   selectInput('x', 'x axis', choices = names(mtcars)),
#'   selectInput('y', 'y axis', choices = names(mtcars)),
#'   uiOutput('copy_code_btn'),
#'   plotOutput('plot')
#' )
#' 
#' srv <- function(input, output, session) {
#'   output$plot <- renderPlot({
#'     plot(x = mtcars[[input$x]],
#'          y = mtcars[[input$y]])
#'   })
#'   output$copy_code_btn <- renderUI(clipCodeButton(srv))
#' }
#' 
#' shinyApp(ui, srv)
#' }
#'
#' @importFrom shiny tagList tags
#'
#' @export
clipCodeButton <- function(server, label = NULL, icon = NULL, 
    modal = FALSE, id = "clipboard_btn", envir = parent.frame(), 
    text = get_code(server, envir = envir)) {

  check_shiny()
  
  if (missing(text) && !missing(server)) text <- get_code(server, envir = envir)
  
  # TODO: 
  #   Would be nice if this could be unique per output, but it generates a new 
  #   ID on every update
  # if (is.null(id)) id <- shiny:::createUniqueId(8, "clipboard_btn_")
  
  if (is.null(icon)) icon <- shiny::icon("clipboard")
  modal_script <- 'new Clipboard("#%s", { container: document.getElementById("shiny-modal") });'
  free_script  <- 'new Clipboard("#%s")'

  shiny::tagList(
    shiny::tags$button(id = id, type = "button",
      class = "btn btn-default action-button shiny-bound-input",
      "data-clipboard-text" = text,
      icon,
      label
    ),
    if (modal) {
      shiny::tags$script(sprintf(modal_script, id))
    } else {
      shiny::tags$script(sprintf(free_script, id))
    }
  )
}
