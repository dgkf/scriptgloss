#' @export
callStaticModule <- function(module) {
  UseMethod("callStaticModule", module)
}

#' @export
callStaticModule.scriptgloss_module <- function(module) {
  attr(module, "return")
}

#' @export
module <- function(module, ...) {
  UseMethod("module", module)
}

#' @export
module.scriptgloss_module <- function(module, id) {
  if (missing(id))
    attr(module, "submodules")
  else
    attr(module, "submodules")[[id]]
}

#' @export
get_output <- function(module, ...) {
  UseMethod("get_output", module)
}

#' @export
get_output.scriptgloss_module <- function(module, output) {
  output_val <- module[[output]]
  if (is.function(output_val)) output_val()
  else output_val
}