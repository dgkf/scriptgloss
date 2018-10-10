#' Pull names of all top-level output items
#'
#' @param x code block from which output items will be extracted
#'
#' @return names of top level output functions
#'
#' @examples
#' code <- quote({
#'   output$first <- 'first'
#'   output$second <- function() print('second')
#'   output$third <- 5
#' })
#'
#' scriptgloss:::extract_output_names(code)
#'
extract_output_names <- function(x) {
  Filter(Negate(is.null), Map(function(c) {
    if (is.call(c) && c[[1]] == "<-" &&
        is.call(c[[2]]) && c[[2]][[1]] == "$" && c[[2]][[2]] == "output") {
      as.character(c[[2]][[3]])
    } else NULL
  }, as.list(x)))
}
