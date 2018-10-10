#' Grab names of all outputs generated in a list of code
#'
#' @param codelist a list of top level calls to be scraped for assignment to the
#'   shiny server output list
#'
#' @return a list of output names
#' @examples
#' q <- quote({
#'   a <- 1
#'   output$y <- "z"
#'   b <- 2
#'   output$x <- a
#' })
#' 
#' scriptgloss:::shiny_output_names(q)
shiny_output_names <- function(codelist) {
  lapply(lapply(Filter(is_shiny_output_assignment, codelist), "[[", 2), "[[", 3)
}



#' Test if a given expression is an assignment call to the shiny output list
#'
#' @param line a language object
#'
#' @return logical value indicating whether this language object represents an
#'   assignment to the shiny output list
is_shiny_output_assignment <- function(line) {
  is.call(line) && 
  line[[1]] == "<-" && 
  length(line[[2]]) > 1 && 
  line[[2]][1:2] == quote(output$a)[1:2]
}