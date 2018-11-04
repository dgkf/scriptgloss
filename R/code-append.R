#' Add code to code block
#'
#' @param x existing code block to append to
#' @param code additional list of code to append
#' @param after position at which to append code
#'
#' @return the initial code block with additional code inserted at specified
#'   \code{after} position
#'
#' @examples
#' code1 <- quote({
#'   a <- 1
#'   b <- 2
#'   e <- 5
#' })
#'
#' code2 <- quote({
#'   c <- 3
#'   d <- 4
#' })
#'
#' scriptgloss:::append_code(code1, as.list(code2)[-1], 2)
#'
append_code <- function(x, code, after = length(x)) {
  if (!length(code)) return(x)
  append(as.list(x), as.list(code), after)
}



#' Append declarations to code block
#'
#' @description appends all ellipses or dots arguments to code at specified
#'   location, reconstructing code to initialize variables in the code block.
#'
#' @param x code block to append to
#' @param ... a named list of variables to create declarations for. currently
#'   only objects of a subset of types get instantiated: 
#'   \itemize{
#'   \item{reactivevalues (via \code{\link[shiny]{reactiveValuesToList}()})}
#'   \item{atomic} 
#'   \item{numeric} 
#'   \item{character}
#'   }
#' @param after position at which to insert declaration code
#' @param dots alternative passing of ellipses arguments
#'
#' @return reconstructed code with declarations inserted
#'
#' @examples
#' code <- quote({
#'   a <- tolower(a)
#'   b <- b[[1]]
#'   c <- tolower(c)
#'   print(sprintf('this is %s %dst %s', a, b, c))
#' })
#'
#' scriptgloss:::append_declaration(as.list(code)[-1], 
#'     a = 'my', 
#'     b = list(1,2,3),
#'     c = 'example', after = 0)
#'
#' @importFrom shiny reactiveValuesToList
#'
append_declaration <- function(x, ..., after = length(x), dots = list()) {
  check_shiny()
  dots <- rev(c(dots, list(...)))
  for (d in names(dots)) {
    # handle desired shiny-specific variable types
    if ("reactivevalues" %in% class(dots[[d]])) 
      dots[[d]] <- shiny::reactiveValuesToList(dots[[d]])
    
    # reject additional shiny-specific variables
    if (any(class(dots[[d]]) %in% names(getNamespace("shiny")))) next
    
    # otherwise reconstruct code to initialize variable
    data_constructor <- getInitializationCode(dots[[d]])
    x <- append(x, bquote(.(as.name(d)) <- .(data_constructor)), after)
  }
  
  x
}



#' Add calls to output list functions
#'
#' @description add calls such as \code{output$plot()} to a code block. helpful
#'   for adding function call to a independent R script such that output gets
#'   drawn at end of script.
#'
#' @param x code block to which output calls will be appended
#' @param ... names of outputs to be called as character
#' @param dots optionally pass ellipses arguments as list
#' @param after position at which to create calls
#'
#' @return code block with calls to output functions appended
#'
#' @examples
#' code <- quote({
#'   a <- 1
#' })
#'
#' scriptgloss:::append_output_calls(code, 'plot', 'table', 'text')
#'
append_output_calls <- function(x, ..., dots = list(), after = length(x)) {
  dots <- c(list(...), as.list(dots))
  append(x,
    Map(function(i) call("$", as.name("output"), call(i)), dots),
    after = after)
}