#' Shake syntax tree for shiny output-relevant code
#'
#' @param code a list of lines of code for shaking
#' @param ... names of elements of shiny output object to shake for
#' @param dots additional ellipses arguments as list
#' @param keep_returns whether to prune return statements if they arent' needed
#'   for generated specified outputs. this is generally useful in situations
#'   where a specific output is desired, but unwanted when generated compelete
#'   nested module code.
#' @param verbose logical indicating whether to print descriptive code output
#'   indicating which lines were selected for inclusion
#'
#' @return the code_list with isolated expressions in the syntax tree pruned
#'
#' @examples
#' code <- quote({
#'   library(utils)
#'   require(stats)
#'   
#'   output <- list()
#'   
#'   a <- 'This is unused in global scope'
#'   x <- 'this is a default value'
#'   output$a <- function(a, b = x) {
#'     return(a)
#'   }
#'   
#'   output$b <- function(c) {
#'     return(c)
#'   }
#' })
#' 
#' scriptgloss:::shake_shiny_outputs(code, "b")
#' 
#' @importFrom CodeDepends readScript inputCollector getInputs getDependsThread
#' @importFrom grDevices rgb
#' @importFrom methods show
#' @importFrom utils capture.output
#' 
shake_shiny_outputs <- function(code, ..., dots = list(),
  keep_returns = FALSE, verbose = getOption('scriptgloss.verbose')) {
  
  outputs <- c(list(...), as.list(dots))
  if (!length(outputs)) return(code)
  
  # always keep assignment to output variable
  code_thread <- which(sapply(
    code, 
    function(i) is.call(i) && i[1:2] == quote(output <- NULL)[1:2], 
    simplify = "logical"))
  
  # always keep return statements and last expression
  #
  # TODO: 
  #   Currently only captures top level return statements, but will
  #   eventually need to capture any nested return statements as well
  #  
  if (keep_returns) 
    code_thread <- c(
      code_thread, 
      which(sapply(
        code,
        function(i) is.call(i) && i[[1]] == "return",
        simplify = "logical")),
      length(code))
  
  # 
  # TODO: 
  #   Keep all syntax tree branches with a library/require/install call
  #   
  # FOR NOW:
  #   keep all top level calls to 'library', 'install.packages', 'require' or
  #   'devtools::...'
  # 
  code_thread <- c(code_thread, which(sapply(code, function(i) 
    is.call(i) && (
      i[[1]] == quote(require) || 
      i[[1]] == quote(library) || 
      i[[1]] == quote(install.packages) ||
      as.character(i) %in% getNamespaceExports("devtools") ||
      (length(i[[1]]) > 1 && i[[1]][1:2] == call("::", as.name("devtools"))[1:2])
    ))))
  
  # add all inputs to the last expression to outputs
  if (keep_returns)
    outputs <- c(outputs, CodeDepends::getInputs(code[[length(code)]])@inputs)
  
  # shake tree
  code_thread <- Reduce(c, 
    Map(function(n) {
        scr <- CodeDepends::readScript(txt = code)
        col <- CodeDepends::inputCollector(
          inclPrevOutput = TRUE, 
          funcsAsInputs = TRUE)
        
        incol <- CodeDepends::getInputs(scr, collector = col)

        tryCatch(
          CodeDepends::getDependsThread(n, incol), 
          error = function(e) integer())
      }, 
      outputs), 
    init = code_thread)
  code_thread <- sort(unique(code_thread))
  
  # display info (possibly use crayon, but has fallback for standard output)
  if (isTRUE(verbose)) {
    if (requireNamespace("crayon", quietly = TRUE)) { 
      crayon::make_style(ltGrey = grDevices::rgb(0.7, 0.7, 0.7))
      for (i in seq_along(code))
        if (i %in% code_thread)
          message(crayon::black(as_shown(code[[i]])), '\n')
      else
        message(crayon::style(as_shown(code[[i]]), "ltGrey"), '\n')
    } else {
      for (i in seq_along(code))
        if (i %in% code_thread)
          message(
            paste(
              "+", 
              utils::capture.output(methods::show(code[[i]])), 
              collapse = "\n"), 
            "\n")
      else
        message(
          paste(
            "-", 
            utils::capture.output(methods::show(code[[i]])), 
            collapse = "\n"), 
          "\n")
    }
    message('\n\n')
  }
  
  code[code_thread]
}