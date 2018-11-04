#' Partially evaluate a code block with given variable values
#' 
#' @param code the code block to use as the basis for evaluation
#' @param ... named variables and values to be evaluated within the code
#' @param dots optionally specify additional ellipses names as list
#' @param envir the environment in which subtrees should be evaluated. If
#'   missing (the default) an environment as a direct child of the base
#'   environment is created with only declarations of the ellipses arguments and
#'   used for evaluation of the tree.
#'
#' @return the input code block with specified variables partially applied
#'
#' @examples
#' codeblock <- quote({
#'   # inserting values into a code block
#'   my_function_1 <- function() {
#'     print(paste(input$a, input$b, other_var))
#'   }
#' 
#'   my_function_2 <- function() {
#'     print(input$a)
#'     print(input$b)
#'   }
#'   
#'   # example of behavior when a value is not present
#'   my_function_2 <- function() {
#'     print(input$c + other_var)
#'   }
#'   
#'   # example of variable getting declared in a local frame
#'   my_function_3 <- function() {
#'     input <- 3
#'     print(input)
#'   }
#'   
#'   # example of a variable getting declared in a parent frame
#'   input <- 4
#'   my_function_4 <- function() {
#'     print(input)
#'   }
#' })
#' 
#' 
#' codeblock <- quote({
#'   output$test <- function() { print("a") }
#'   output$test()
#' })
#' 
#' scriptgloss:::propegate_values(
#'     codeblock, 
#'     input = list(
#'       a = list(1, 2, 3, 4), 
#'       b = "b"))
#' 
#' @importFrom CodeDepends readScript getDetailedTimelines
#' 
propegate_values <- function(code, ..., dots = list(), envir = NULL) {
  dots <- c(as.list(dots), list(...))
  
  # create an empty environment to evaluate values within
  if (missing(envir) || is.null(envir))
    envir <- new.env(parent = baseenv())
  
  # assign values in new environment
  Map(function(x, value) assign(x, value, envir = envir), 
    x = names(dots), 
    value = dots)
  
  # logical list indicating whether syntax elements are 'basic' elements
  basic_syntax <- lapply(as.list(code), function(i) {
    any(is.primitive(i), is.name(i), is.symbol(i), is.atomic(i))
  })
  
  # attempt to fill in code with evaluable atomic output
  if (all(as.logical(basic_syntax)))
    try(if (is.atomic(e <- eval(code, envir = envir))) return(e), silent = TRUE)
  
  # otherwise, continue parsing relevant parts of the syntax tree
  if (is.call(code) || is.expression(code) || is.pairlist(code) || is.list(code)) {
    # get script metadata 
    scr      <- CodeDepends::readScript(txt = as.list(code))
    timeline <- CodeDepends::getDetailedTimelines(scr, vars = names(envir))
    scr      <- scr[] # coerce to list
  
    # find applicable lines for variable instance before next declaration
    # (for single variable, general form below)
    # lines <- cumsum(lines$defined) == 0 & lines$used 
    lines <- rowSums(with(timeline,
      matrix(ave(defined, var, FUN=cumsum) == 0 & used, 
        ncol = length(unique(var))))) > 0
    
    # recursively iterate over relevant lines
    scr[lines] <- lapply(scr[lines], propegate_values, envir = envir)
    if (is.list(code)) scr else as.call(scr)
  } else if (is.atomic(code) || is.name(code)) { 
    code
  } else { 
    stop("Don't know how to handle type ", typeof(code), call. = FALSE)
  }
}

