#' @title Call a static representation of a shiny module
#' 
#' @description An S3 generic for returning a statically
#' 
#' @param module a module object
#' @return the return of calling a shiny module
#' 
#' @export
callStaticModule <- function(module) {
  UseMethod("callStaticModule", module)
}

#' @export
callStaticModule.scriptgloss_module <- function(module) {
  attr(module, "return")
}



#' A function for acccessing a shiny submodule
#' 
#' @param module the module object to search for submodules
#' @param id the submodule id to return. if not provided, a list of submodules
#'   is returned.
#' @return a submodule or list of submodules from the provided module
#' 
#' @export
submodule <- function(module, id) {
  UseMethod("submodule", module)
}

#' @export
submodule.scriptgloss_module <- function(module, id) {
  if (missing(id))
    attr(module, "submodules")
  else
    attr(module, "submodules")[[id]]
}



#' Retrieve an output from a shiny module
#' 
#' @param module the module object to search for outputs
#' @param output an optional name of the output to retrieve. if not provided, a
#'   list of outputs is returned.
#' @return an output or list of outputs from the specified module
#' 
#' @export
get_output <- function(module, output) {
  UseMethod("get_output", module)
}

#' @export
get_output.scriptgloss_module <- function(module, output) {
  if (missing(output)) {
    c(module)
  } else {
    output_val <- module[[output]]
    if (is.function(output_val)) output_val()
    else output_val
  }
}