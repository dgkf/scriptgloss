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
purge_shiny_code <- function(x, envir = parent.frame()) {
  if (is.call(x)) {
    # find assignment to the shiny output list and replace with nullary function
    if (any(gsub('^shiny:{2,3}', '', as.character(x[[1]])) %in% 
        grep("^render", getNamespaceExports("shiny"), value = TRUE))) {
      x <- match.call(definition = eval(x[[1]]), call = x)
      x <- call("function", NULL, purge_shiny_code(as.list(x)$expr, envir = envir))
      x
      
    # find reactive({ ... }) calls and convert them to function() { ... }
    } else if (grepl_expr('^(shiny:{2,3})?reactive$', x[[1]])) {
      x <- match.call(definition = eval(x[[1]]), call = x)
      call("function", NULL, purge_shiny_code(x$x, envir = envir))
    
    # find reactiveValuesToList(...) and convert to list(...)
    } else if (grepl_expr('^(shiny:{2,3})?reactiveValuesToList$', x[[1]])) {
      do.call("call", c("list", eval(x, envir = envir)))
      
    # find reactiveVal(...) calls and convert them to function() { ... }
    } else if (x[[1]] == '<-' && is.call(x[[3]]) && grepl_expr('^(shiny:{2,3})?reactiveVal$', x[[3]][[1]])) {
      val <- eval(call(as.character(x[[2]])), envir = envir)
      x[[3]] <- call("function", NULL, val)
      x 
               
    # find reactiveValues(...) calls and convert them to list(...)
    } else if (x[[1]] == '<-' && is.call(x[[3]]) && grepl_expr('^(shiny:{2,3})?reactiveValues$', x[[3]][[1]])) {
      vals <- reactiveValuesToList(eval(x[[2]], envir = envir))
      x[[3]] <- do.call("call", c("list", vals))
      x
      
    # remove observe({ ... }) calls
    } else if (any(gsub('^shiny:{2,3}', '', as.character(x[[1]])) %in% 
        grep("^observe", getNamespaceExports("shiny"), value = TRUE))) {
      NULL
      
    } else {
      # 'deshine' contents of call stack, removing culled branches (NULLs)s
      as.call(Filter(Negate(is.null), lapply(x, purge_shiny_code, envir = envir)))
    }
  
  } else if (is.expression(x)) 
    as.expression(purge_shiny_code(x[[1]], envir = envir))
  else if (is.atomic(x) || is.name(x)) 
    x
  else if (is.pairlist(x)) 
    as.pairlist(lapply(x, purge_shiny_code, envir = envir))
  else if (is.list(x))
    lapply(x, purge_shiny_code, envir = envir)
  else stop("Don't know how to handle type ", typeof(x), call. = FALSE)
}



grepl_expr <- function(pattern, x, ...) {
  grepl(pattern, paste0(capture.output(x), collapse = '\n'), ...)
}