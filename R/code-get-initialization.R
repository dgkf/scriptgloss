#' Code generation generic for arbitrary datatypes
#'
#' @param obj an object for which initialization code must be reconstructed
#' @param name an optional variable name for a more helpful warning message
#'
#' @return quoted code that can be used to instantiate the object
#' @export
getInitializationCode <- function(obj, name = NULL) {
  UseMethod("getInitializationCode")
}


setGeneric("getInitializationCode")


#' Code generation generic default behavior
#'
#' @inheritParams getInitializationCode
#' @return quoted code that can be used to instantiate the object
#' 
#' @importFrom utils capture.output
#' 
#' @export
getInitializationCode.default <- function(obj, name = NULL) {
  tryCatch({ 
    control = c(
      "keepNA", 
      "keepInteger", 
      if (is.list(obj)) NULL else "showAttributes")
    parse(text = utils::capture.output(dput(obj, control = control)))[[1]]
  },
    error = function(e) { 
      warning(
        'While rebuilding shiny variable initializations, ',
        sprintf('variable %sof class %s', 
          if (!is.null(name)) paste0('"', name, '" ') else '',
          paste0('"', paste(as.character(class(obj)), collapse = ", "), '"')),  
        ' could not be reconstructed. ',
        'To enable variable reconstruction, add a method dispatched call to ',
        sprintf('"getInitializationCode.%s()"', class(obj)[[1]]),
        ' for your object.')
      quote(NULL)
    }
  )
}

