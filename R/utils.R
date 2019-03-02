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
#' @param calling_f a function name which triggered the check_shiny call.
#'   defaults to the name of the calling function.
#' @param ignore logical indicating whether to return TRUE even if shiny is not
#'   currently running (generally only used for testing purposes.)
#'
#' @return logical indicating whether shiny namespace is available
#' 
#' @importFrom shiny isRunning
#' 
check_shiny <- function(calling_f = as.list(sys.call(-1L))[[1]], 
    ignore = getOption("scriptgloss.testmode")) {
  
  if (isTRUE(ignore) || shiny::isRunning()) {
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
  gsub("(^\\n+)|(\\n+$)", "", paste0(capture.output(x), collapse = "\n"))
}



#' Helper to debug shinytests so they work interactively as well as during test
#'
#' @param path a path within the tests/shinytest/ directory
#'
#' @return a path that works irrespective of how the code is executed
#' 
shinytest_path <- function(path) {
  # catches
  #   * devtools::test()
  #   * testthat::test_dir(testthat::test_path())
  #   * testthat::auto_test_package()
  #   * covr::package_coverage()
  
  top_level_call_f <- capture.output(as.list(sys.calls()[[1]])[[1]])
  
  if (any(grepl("scriptgloss", top_level_call_f)) ||
      !any(grepl("test", top_level_call_f))) {
    file.path(".", "tests", "shinytest", path)
  } else {
    file.path("..", "shinytest", path)
  }
}