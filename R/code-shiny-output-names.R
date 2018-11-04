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



#' Originally started mocking up this function in order to tell whether a
#' function was a shiny server function (for example, for a module created in the
#' header of the file or sourced from another file). However, after some
#' consideration, this method would fail to include module code loaded from
#' library calls into generated code.
#' 
#' Instead, I'll plan to evaluate the content of the module server name and build
#' a function dynamically. Eventually, if the module is also declared in the
#' parent scope, this will get shaken anyways.
#'
# is_shiny_server <- function(x, fs = "^(observe|react|render)") { # search for
# matching shiny functions shinyfs <- Reduce(union, sapply(fs, function(i) {
# grep(i, getNamespaceExports("shiny"), value = TRUE) }))
#
# # append shiny namespace as option shinyfs <- c(shinyfs, paste0("shiny::",
# shinyfs), paste0("shiny:::", shinyfs))
#
# # strip out function header if included if (as.list(x)[[1]] == "function") x
# <- as.list(x)[[3]]
#
# # test if any top level expressions are calls to shiny functions
# any(sapply(as.list(x), function(i) { if (is.call(i)) { # top level `observe`
# calls if (as.character(i[[1]]) %in% shinyfs) return(TRUE) # things like
# `output$something <- renderPlot({...})` if (i[[1]] == "<-" && is.call(i[[3]])
# && as.character(i[[3]][[1]]) %in% shinyfs) return(TRUE) } FALSE }, simplify =
# "logical")) }
