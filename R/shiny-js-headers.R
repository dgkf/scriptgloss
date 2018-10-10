#' HTML <script> calls for javascript dependencies
#' 
#' @inheritDotParams prismDependencies
#' 
#' @export
scriptglossJS <- function(...) shiny::tagList(
  prismDependencies(...),
  clipboardjsDependencies()
)