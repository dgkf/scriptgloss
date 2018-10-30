#' Generate R code from shiny server code
#'
#' @description Takes in a shiny server function, searches working directory for
#'   \code{app.R} and \code{global.R} files, and reconstructs an independent R
#'   script from the server components.
#'
#' @param server the server function to generate an R script for
#' @param ... names of output names for which to build code
#' @param dots optionally pass ellipses names of outputs as list
#' @param call_outputs whether calls to the specified outputs should be appended
#'   at the end of the script
#' @param flatten_outputs whether a singular output should be collapsed into
#'   parent script
#' @param files the filepaths to search for available shiny globally scoped code
#' @param envir the environment in which to search for arguments originally
#'   passed to the server function. A construction of those arguments as they
#'   exist in the current state will attempt to be built into the generated
#'   script.
#'
#' @return a script representing the current state of the shiny app, allowing
#'   for independent reproduction of the shiny outputs.
#'
#' @examples
#' \dontrun{
#' ## Shiny Example (app.R contents)
#' try(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)), silent = TRUE) # R Studio
#' try(setwd(dirname(dirname(parent.frame(2)$ofile))), silent = TRUE) # running as a script
#' 
#' library(shiny)
#' 
#' my_data <- datasets::mtcars
#' 
#' # additional code before shinyApp call will be captured if in app.R
#' ui <- fluidPage(
#'   selectInput('x', 'x axis', choices = names(my_data)),
#'   selectInput('y', 'y axis', choices = names(my_data)),
#'   plotOutput('plot'),
#'   verbatimTextOutput('code')
#' )
#' 
#' srv <- function(input, output, session) {
#'   output$plot <- renderPlot({
#'     plot(x = my_data[[input$x]],
#'          y = my_data[[input$y]])
#'   })
#'   output$code <- renderPrint({
#'     cat(get_code(srv, 'plot'))
#'   })
#' }
#' 
#' shinyApp(ui, srv)
#' }
#'
#' @export
#'
generate_static_code <- function(server, ..., dots = list(), 
    call_outputs = TRUE, flatten_outputs = TRUE,
    files = file.path(getwd(), c('^app\\.R$', '^global\\.R$')),
    envir = parent.frame(),
    session = get("session", envir = envir)) {
  
  # grab shiny file code
  files <- unlist(lapply(parse_shiny_files(files), top_level_call_list))
  
  # collect arguments passed to the server function, evaluating in server frame
  srv_args <- shiny_server_header_vars(server, envir)
  # srv_args_no_io <- srv_args[!names(srv_args) %in% c("input", "output")]
  srv_args_no_io <- srv_args[!names(srv_args) %in% "output"]
  
  # get list of expressions from server function
  srv_body <- top_level_call_list(body(server))
  
  # get list of requested output names, take all if none are specified
  outputs <- c(list(...), as.list(dots))
  if (!length(outputs)) outputs <- shiny_output_names(srv_body)
  
  # TODO: include input declaration if not all values are atomic,
  #       for now it's assumed all input values are atomic and can be inlined
  #       into code body
  
  # process server code and append global files
  # TODO: purge_shiny_code should convert `callModule` calls to static code,
  #       include handling of `NS` functions
  srv_body <- purge_shiny_code(srv_body) # remove reactives and observers
  # srv_body <- expand_callModule_calls(srv_body, session = session, envir = envir)
  srv_body <- append_declaration(srv_body, dots = srv_args_no_io, after = 0)
  srv_body <- append_code(srv_body, files, after = 0)
  
  # make each output a unique variable instead of part of an output list
  srv_body <- replace_output_w_tmp(srv_body)
  outputs <- sprintf("output__%s__", outputs)
  
  # restructure code by filling in input values and shaking syntax tree
  # srv_body <- propegate_values(srv_body, 
  #   dots = srv_args[!names(srv_args) %in% "output"], 
  #   envir = envir)
  
  # replace output list entries with temporary objects for tree shaking
  srv_body <- shake_shiny_outputs(srv_body, dots = outputs)
  
  # clean up code in situation where only one output is being derived
  if (length(outputs) == 1 && flatten_outputs) {
    srv_body <- flatten_function_body(srv_body, outputs[[1]])
  # alternatively, keep outputs as function definitions and append calls
  } else {
    srv_body <- append_declaration(srv_body, output = list(), after = 0)
    if (call_outputs)
      srv_body <- append_output_calls(srv_body, dots = names(outputs))
  }
  
  # substitute temporary output objects with list entries
  srv_body <- replace_tmp_w_output(srv_body)
  
  return(as.call(c(list(quote(`{`)), srv_body)))
}



#' Parse files for code that is executed before shiny runtime
#'
#' @param files files to consider
#'
#' @return a list of expressions stripped of code 
#' 
parse_shiny_files <- function(
    files = file.path(getwd(), c('app.R', 'global.R'))) {
  
  # filter down to only files that exist
  files <- files[file.exists(files)]
  
  # parse code from discovered files
  files <- lapply(files, function(f) {
    f <- parse(f)
    f <- as.call(c(list(quote(`{`)), f))
    f <- trim_shinyApp(f)
    reduce_nested_syntax(f)
  })
}



#' Grab formals from function environment
#'
#' @param server server function from which to grab formals
#' @param envir environment in which to evaluate server formals
#'
#' @return a named list of server header formal values 
#' 
shiny_server_header_vars <- function(server, envir = parent.frame()) {
  Map(function(n) {
    if (n == 'output') list()
    else eval(bquote(tryCatch(.(as.name(n)), error = function(e) NULL)), envir)
  }, names(formals(server)))
}



#' Convert a code block to a list of language calls
#'
#' @param x code block
#' @return a list of language calls
top_level_call_list <- function(x) {
  if (length(x) > 1 && (x_list <- as.list(x))[[1]] == "{") x_list[-1]
  else list(x)
}