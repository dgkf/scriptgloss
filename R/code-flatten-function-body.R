#' Flatten a single output's function body
#' 
#' When only a single output's code is desired, you can flatten the function
#' body of the nullary output function into the top level of the syntax tree to
#' clean up unnecessary compartmentalization.
#' 
#' With more than one output, you run the risk of introducing namespace
#' conflicts that were otherwise handled by compartmentalizing withing
#' functions.
#'
#' @param code a code block to flatten
#' @param name  output to flatten
#'
#' @return code with last instance of outputs flattened into top level of syntax
#'   tree.
#'
#' @examples
#' codeblock <- quote({
#'   a <- 1
#'   b <- 2
#'   f <- function() {
#'     print(a)
#'     print(b)
#'   }
#' })
#' 
#' scriptgloss:::flatten_function_body(codeblock, "f")
#' 
#' @importFrom utils tail
#' 
flatten_function_body <- function(code, name) {
  src <- CodeDepends::readScript(txt = as.list(code))
  timeline <- CodeDepends::getDetailedTimelines(src, vars = list(name))
  
  output_line <- src[[loc <- utils::tail(which(timeline$defined), 1)]]
  if (is.call(output_line[[3]]) && output_line[[3]][[1]] == "function") {
    output_body <- output_line[[3]][[3]]
    if (output_body[[1]] == "{") output_body <- as.list(output_body)[-1]
    append(src[-loc], output_body, loc)
  } else
    src
}



#' Flatten multiple function bodies into parent scope
#' 
#' With more than one output, you run the risk of introducing namespace
#' conflicts that were otherwise handled by compartmentalizing withing
#' functions.
#'
#' @param code a code block to flatten
#' @param ...  outputs to flatten. only guarenteed to produce appropriate code
#'   for a single value, though it will work as intended for more than one
#'   output.
#' @param dots optionally specify ellipses arguments as list
#'
#' @return code with last instance of outputs flattened into top level of syntax
#'   tree.
#'   
flatten_function_bodies <- function(code, ..., dots = list()) {
  dots <- c(list(...), as.list(dots))
  for (name in dots) code <- flatten_function_body(code, name)
  code
}