#' Unnest code blocks unnecessarily wrapped in curly brackets
#'
#' @param x quoted code to process
#' @param depth level of nesting during processing. Used to track code location
#'   information during recursive calls.
#'
#' @return code which is functionally equivalent but with unnecessary curly
#'   brackets removed
#'   
#' @examples
#' q <- quote({
#'   {
#'     {
#'       a <- 1
#'     }
#'   }
#'   x <- function(a = 1, b = { z <- a; z + 1 }, c = { z + 1}) {
#'     quote({
#'       a <- 1
#'       {
#'         b <- 2
#'       }
#'     })
#'     a <- 2
#'   }
#' })
#' 
#' scriptgloss:::reduce_nested_syntax(q)
#' 
reduce_nested_syntax <- function(x, depth = 1) {
  if (is.call(x) && x[[1]] == "{")
    if (depth > 1) {
      lapply(as.list(x)[-1], reduce_nested_syntax, depth = depth + 1)
    } else {
      expr <- unlist(lapply(
          as.list(x)[-1], 
          reduce_nested_syntax, 
          depth = depth + 1))
      
      if (length(expr) > 1) as.call(c(as.name("{"), expr))
      else if (length(expr) && !is.na(expr)) as.call(expr[[1]])
      else as.call(as.list(as.name("{")))
    }
  else if (is.call(x))       as.call(lapply(x, reduce_nested_syntax))
  else if (is.expression(x)) as.expression(lapply(x, reduce_nested_syntax))
  else if (is.pairlist(x))   as.pairlist(lapply(x, reduce_nested_syntax))
  else x
}