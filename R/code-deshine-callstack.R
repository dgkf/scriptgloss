#' Remove shiny-specific code from callstack
#'
#' @description walk an expression, call or list, replacing canonical shiny
#'   server functions with R script equivalent code.
#'
#' @note specifically, a few shiny functions built around a reactive javascript
#'   context are reconstructed as base R functions:
#'   \itemize{
#'   \item{\code{my_reactive_value <- reactive({ ... })} will become
#'         \code{my_reactive_value <- function() { ... }}}
#'   \item{All shiny render functions
#'         (\code{output$my_output <- render*({ ... })}) will become
#'         \code{output$my_output <- function() { ... }}}
#'   \item{calls to \code{observeEvent()} becomes \code{NULL}}
#'   \item{calls to \code{observe()} becomes \code{NULL}}
#'   }
#'
#' @param x a call, or expression to strip of shiny-specific functions
#'
#' @return the same call or expression, with shiny runtime-specific
#'   functionality stripped and replaced with code that will evaluate within an
#'   R script.
#'
#' @examples
#' library(shiny)
#' 
#' code <- quote({
#'   output$plot <- renderPlot({
#'     plot(x = mtcars[[input$x]],
#'          y = mtcars[[input$y]])
#'   })
#'   observeEvent(input$give_me_a_happy_message, {
#'     "you're a great person!"
#'   })
#'   output$code <- renderPrint({
#'     generate_static_code(srv, 'plot')
#'   })
#' })
#'
#' scriptgloss:::purge_shiny_code(code)
#'
purge_shiny_code <- function(x) {
  if (is.call(x))
    
    # find assignment to the shiny output list and replace with nullary function
    if (x[[1]] == '<-' && is.call(x[[2]]) && x[[2]][[2]] == 'output') {
      x[[3]] <- match.call(definition = eval(x[[3]][[1]]), call = x[[3]])
      x[[3]] <- call("function", NULL, purge_shiny_code(as.list(x[[3]])$expr))
      x
      
    # find reactive({ ... }) calls and convert them to function() { ... }
    } else if (x[[1]] == 'reactive') {
      x <- match.call(definition = eval(x[[1]]), call = x)
      call("function", NULL, purge_shiny_code(x$x))
      
    # remove observe({ ... }) calls
    } else if (any(as.character(x[[1]]) %in% 
        grep("^observe", getNamespaceExports("shiny"), value = TRUE))) {
      NULL
      
    } else {
      # 'deshine' contents of call stack, removing culled branches (NULLs)s
      as.call(Filter(Negate(is.null), lapply(x, purge_shiny_code)))
    }
  
  else if (is.expression(x)) 
    as.expression(purge_shiny_code(x[[1]]))
  else if (is.atomic(x) || is.name(x)) 
    x
  else if (is.pairlist(x)) 
    as.pairlist(lapply(x, purge_shiny_code))
  else if (is.list(x))
    lapply(x, purge_shiny_code)
  else stop("Don't know how to handle type ", typeof(x), call. = FALSE)
}
