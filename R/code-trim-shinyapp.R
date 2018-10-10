#' Trim code after shinyApp() call
#'
#' @description Replaces shinyApp() call with \code{NULL} and removes all
#'   following code from a block of code. This is particularly useful to
#'   recreate the server state as none of the code after a shinyApp() call will
#'   be reached.
#'
#' @param x code or expression to strip of code at the shinyApp() call
#'
#' @return the code or expression with all code at and beyond the shinyApp()
#'   call removed.
#'
#' @examples
#' code <- quote({
#'   ui <- fluidPage(
#'     selectInput('x', 'x axis', choices = names(mtcars)),
#'     selectInput('y', 'y axis', choices = names(mtcars)),
#'     plotOutput('plot'),
#'   )
#'   
#'   srv <- function(input, output, session) {
#'     output$plot <- renderPlot({
#'       plot(x = mtcars[[input$x]],
#'         y = mtcars[[input$y]])
#'     })
#'   }
#'
#'   shinyApp(ui, srv)
#'
#'   print('this is some code after the shinyApp() call')
#'   
#'   a <- 1
#'   b <- 2
#' })
#'
#' scriptgloss:::trim_shinyApp(code)
#' 
#' 
#' 
#' code2 <- quote({
#'   ui <- fluidPage(
#'     selectInput('x', 'x axis', choices = names(mtcars)),
#'     selectInput('y', 'y axis', choices = names(mtcars)),
#'     plotOutput('plot'),
#'   )
#'   
#'   srv <- function(input, output, session) {
#'     output$plot <- renderPlot({
#'       plot(x = mtcars[[input$x]],
#'         y = mtcars[[input$y]])
#'     })
#'   }
#'
#'   a <- 1
#'
#'   if (a == 1) {
#'     shinyApp(ui, srv)
#'   } else {
#'     print("This app never got launched!")
#'   }
#'
#'   print('this is some code after the shinyApp() call')
#'   
#'   b <- 2
#' })
#'
#' scriptgloss:::trim_shinyApp(code2)
#' 
#' 
#' @importFrom utils head
#' 
trim_shinyApp <- function(x) {
  if (is.call(x)) {
    
    # mark shinyApp call as NULL, then filter NULL & after from
    if (x[[1]] == 'shinyApp' || x[[1]] == quote(shiny::shinyApp)) {
      x <- NULL
    } else {
      call_list <- lapply(x, trim_shinyApp)
      null_idx <- which(unlist(Map(is.null, call_list)))
      null_idx <- utils::head(null_idx, 1)
      if (length(null_idx)) as.call(call_list[1:null_idx])
      else as.call(call_list)
    }
    
  } else if (is.expression(x)) as.expression(lapply(x, trim_shinyApp))
  else if (is.atomic(x) || is.name(x)) x
  else if (is.pairlist(x)) as.pairlist(lapply(x, trim_shinyApp))
  else stop("Don't know how to handle type ", typeof(x), call. = FALSE)
}