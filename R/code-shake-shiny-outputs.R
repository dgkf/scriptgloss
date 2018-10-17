#' Shake syntax tree for shiny output-relevant code
#'
#' @param code a list of lines of code for shaking
#' @param ... names of elements of shiny output object to shake for
#' @param dots additional ellipses arguments as list
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
  verbose = getOption('scriptgloss.verbose')) {
  
  outputs <- c(list(...), as.list(dots))
  if (!length(outputs)) return(code)
  
  # message(paste(capture.output(code), collapse = "\n"))
  # message("[[[DONE]]]")
  
  # always keep assignment to output variable
  code_thread <- which(sapply(
    code, 
    function(i) is.call(i) && i[1:2] == quote(output <- NULL)[1:2], 
    simplify = "logical"))
  
  # always keep return statements
  code_thread <- c(code_thread, which(sapply(
    code,
    function(i) is.call(i) && i[[1]] == "return",
    simplify = "logical")))
  
  # TODO: 
  #   Keep all syntax tree branches with a library/require/install call
  # FOR NOW:
  #   keep all top level calls to 'library', 'install.packages', 'require' or
  #   'devtools::...'
  code_thread <- c(code_thread, which(sapply(code, function(i) 
    is.call(i) && (
      i[[1]] == quote(require) || 
      i[[1]] == quote(library) || 
      i[[1]] == quote(install.packages) ||
      as.character(i) %in% getNamespaceExports("devtools") ||
      (length(i[[1]]) > 1 && i[[1]][1:2] == call("::", as.name("devtools"))[1:2])
    ))))

  code2 <<- code
  code_thread2 <<- code_thread
  outputs2 <<- outputs
  
  # code <- code2
  # code_thread <- code_thread2
  # outputs <- outputs2
  
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
          cat(crayon::black(as_shown(code[[i]])), '\n')
      else
        cat(crayon::style(as_shown(code[[i]]), "ltGrey"), '\n')
    } else {
      for (i in seq_along(code))
        if (i %in% code_thread)
          cat(
            paste(
              "+", 
              utils::capture.output(methods::show(code[[i]])), 
              collapse = "\n"), 
            "\n")
      else
        cat(
          paste(
            "-", 
            utils::capture.output(methods::show(code[[i]])), 
            collapse = "\n"), 
          "\n")
    }
    
    cat('\n\n')
  }
  
  code[code_thread]
}