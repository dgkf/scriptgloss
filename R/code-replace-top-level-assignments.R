#' Replace top level output$name variable assignments with output__name__
#'
#' @details During syntax tree shaking, list datastructures will retain any code
#'   affecting any single list element. In processing Shiny outputs, this
#'   results in retention of all output code even if only a subset of outputs is
#'   desired. To work around this behavior, list elements can be replaced with
#'   unique identifiers to decouple individual list elements.
#'
#' @param code code to modify
#'
#' @return code with assignments replaced
#'
#' @examples
#' q <- quote(output$name <- 3)
#' 
#' scriptgloss:::replace_output_w_tmp(q)
#' # {
#' #   output__name__ <- 3
#' # }
#' 
replace_output_w_tmp <- function(code) {
  replace_top_level_assignments(
    code,
    function(i) is.call(i) && i[1:2] == quote(output$a)[1:2],
    function(i) as.name(sprintf("output__%s__", as.character(i[[3]])))
  )
}



#' Replace top level output__name__ variable assignments with output$name
#'
#' @param code code to modify
#'
#' @return code with assignments replaced
#'
#' @examples
#' q <- quote(output__name__ <- 3)
#' 
#' scriptgloss:::replace_tmp_w_output(q)
#' # {
#' #   output$name <- 3
#' # }
#' 
replace_tmp_w_output <- function(code) {
  replace_top_level_assignments(
    code,
    function(i) is.name(i) && grepl("output__.*__", as.character(i)),
    function(i) { 
      q <- quote(output$a)
      q[[3]] <- as.name(gsub("output__(.*)__", "\\1", as.character(i)))
      q
    })
}



#' Replace top level assignments
#'
#' @param code list of expressions to modify or quoted code block
#' @param from_func a function accepting a single argument, the top level
#'   expression, which will return \code{TRUE} if top level assignment
#'   expression should be modified by \code{to_func}
#' @param to_func  a function accepting a single argument, the expression being
#'   assigned to, defining how a top level assignment expression should be
#'   modified
#'
#' @return modified code block as a list of top level expressions
#' 
#' @examples
#' q <- quote({
#'   a <- list()
#'   a$first <- "1st!"
#'   untouched <- 1
#'   a$second <- "2nd!"
#'   untouched <- 2
#'   a
#' })
#' 
#' scriptgloss:::replace_top_level_assignments(
#'   q, 
#'   function(i) is.call(i) && length(i) >= 3 && i[[1]] == quote(a$a)[[1]],
#'   function(i) as.name(sprintf(
#'     "__%s__%s__", 
#'     as.character(i[[2]]), 
#'     as.character(i[[3]])))
#' )
#'
replace_top_level_assignments <- function(code, from_func, to_func) {
  Map(function(i) {
    if (is.call(i) && (i[[1]] == "<-" || i[[1]] == "=") && from_func(i[[2]]))
      i[[2]] <- to_func(i[[2]])
    else if (is.call(i) && from_func(i[[1]]))
      i[[1]] <- to_func(i[[1]])
    i
  }, code)
}