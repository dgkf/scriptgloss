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



#' Verify that shiny can be used in current scope for modal dialog display
#'
#' @return logical indicating whether shiny namespace is available
#' 
check_shiny <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    warning("shiny must be available to use the showCodeButton function.")
    FALSE
  } else {
    TRUE
  }
}