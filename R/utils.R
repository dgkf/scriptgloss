#' Capture and return show generic output
#'
#' @param x object to show
#'
#' @return a character vector representing 
#' 
#' @importFrom methods show
#' @importFrom utils capture.output
#'
as_shown <- function(x) {
  paste(utils::capture.output(methods::show(x)), collapse = "\n")
}



#' Verify that shiny can be used in current scope
#'
#' @return logical indicating whether shiny namespace is available
#' 
#' @importFrom shiny isRunning
#' 
check_shiny <- function(calling_f = as.list(sys.call(-1L))[[1]], 
    ignore = getOption("scriptgloss.testmode")) {
  
  if ((!is.null(ignore) && ignore) || shiny::isRunning()) {
    TRUE
  } else {
    stop(sprintf(
      "function '%s' can only be used within a running shiny app.", 
      capture.output(calling_f)))
    FALSE
  }
}



#' Mimic shiny::verbatimTextOutput print style
#'
#' @param x object to print
#'
#' @return a character element of verbatim text output
#' 
show_as_verbatim_text <- function(x) {
  trimws(paste0(capture.output(x), collapse = "\n"))
}



#' Helper to debug shinytests so they work interactively as well as during test
#'
#' @param path a path within the tests/shinytest/ directory
#'
#' @return a path that works irrespective of how the code is executed
#' 
shinytest_path <- function(path) {
  if (!any(grepl("devtools|testthat", capture.output(sys.calls()[[1]])))) {
    file.path(".", "tests", "shinytest", path)
  } else {
    file.path("..", "shinytest", path)
  }
}