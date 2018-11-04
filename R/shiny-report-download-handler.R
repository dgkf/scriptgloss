#' Export report as template
#'
#' @param template whisker formatted template rmarkdown file to use as the basis
#'   for report generation
#' @param server server function to use for code generation
#' @param ... additional whisker tags to expand using
#'   \code{\link[knitr]{knit_expand}}
#' @param format the format to be passed to \code{\link[rmarkdown]{render}}
#' @param filename filename to use for generated file. defaults to base filename
#'   of template file
#' @param output_options additional options to be passed to
#'   \code{\link[rmarkdown]{render}}
#' @param envir environment in which to construct report downloader callback
#'
#' @return a shiny downloadHandler to manage building and downloading of report
#'
#' @examples
#' \dontrun{
#' ## MyReport.Rmd (in working directory)
#' # ---
#' # title: "My Report"
#' # ---
#' #
#' # ```{r initialize_code}
#' # {{.code}}
#' # ```
#' #
#' # # My Plot
#' #
#' # ```{r plot}
#' # {{plot}}
#' # ```
#'
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   selectInput('x', 'x axis', choices = names(mtcars)),
#'   selectInput('y', 'y axis', choices = names(mtcars)),
#'   downloadButton("export", "Download Report"),
#'   plotOutput('plot')
#' )
#' 
#' srv <- function(input, output, session) {
#'   output$plot <- renderPlot({
#'     plot(x = mtcars[[input$x]],
#'          y = mtcars[[input$y]])
#'   })
#'   output$export <- report_download_handler('MyReport.Rmd', srv)
#' }
#' 
#' shinyApp(ui, srv)
#' }
#'
#' @importFrom shiny downloadHandler
#' @export
report_download_handler <- function(template, server, ..., 
    format = 'pdf_document',
    filename = paste(Sys.Date(), gsub('\\.[^.]+$','', template), sep = '-'),
    output_options = list(), envir = parent.frame()) {
  
  check_shiny()
  
  # downloadHandler must be declared in server environment to reactively update,
  # build construction and call in the parent frame
  eval(bquote({
    shiny::downloadHandler(
      filename = .(function() {
        if (is.null(format)) format <- 'pdf_document'
        sprintf('%s.%s',
          filename,
          switch(format,
            'word_document' = 'docx',
            'latex_document' = 'tex',
            gsub('_.*', '', format)))
      }),
      content = function(file) {
        .(if (is.null(format)) format <- 'pdf_document')
        report_downloader(
          file,
          report_from_template(.(template), .(server), dots = .(list(...))),
          format = .(format),
          output_options = modifyList(
            list(highlight = 'pygments'),
            .(output_options)
          )
        )
      }
    )
  }), envir = envir)
}



#' Scrape whisker tags that will be pulled from knitr_expand
#'
#' @inheritParams knitr::knit_expand
#'
#' @return the tags which will be pulled for expanding a knitr template
#'
#' @examples
#' textRmd <- "
#' ---
#' title: 'My Report'
#' ---
#' 
#' # {{header_adj}} Header
#' 
#' ```{r}
#' {{whisker_tag1}}
#' ```
#' "
#' 
#' scriptgloss:::knitr_expand_content_list(text = textRmd)
#' #> [1] "header_adj"   "whisker_tag1"
#' 
#' @importFrom stringr str_locate_all str_extract_all
#' @export
#' 
knitr_expand_content_list <- function(file, 
    text = readLines(file, warn = FALSE), delim = c("{{", "}}")) {
  
  if (length(delim) != 2L) 
    stop("\"delim\" must be of length 2")
  
  delim = gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", delim)
  delim = paste0(delim[1L], "((.|\n)+?)", delim[2L])
  txt = paste(text, collapse = "\n")
  loc = stringr::str_locate_all(txt, delim)[[1L]]
  if (nrow(loc) == 0L) 
    return(txt)
  mat = stringr::str_extract_all(txt, delim)[[1L]]
  sub(delim, "\\1", mat)
}



#' Handler for expansion of knitr whisker tags and report generation
#'
#' @param file the file name to be created
#' @param text Rmarkdown file text to use to render notebook. Often the output
#'   of \code{report_from_template}.
#' @param format the output format to be passed to
#'   \code{\link[rmarkdown]{render}}
#' @param output_options additional output options to be passed to
#'   \code{\link[rmarkdown]{render}}
#' 
#' @examples
#' \dontrun{
#' ## MyReport.Rmd (in working directory)
#' # ---
#' # title: "My Report"
#' # ---
#' #
#' # ```{r initialize_code}
#' # {{.code}}
#' # ```
#' #
#' # # My Plot
#' #
#' # ```{r plot}
#' # {{plot}}
#' # ```
#'
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   selectInput('x', 'x axis', choices = names(mtcars)),
#'   selectInput('y', 'y axis', choices = names(mtcars)),
#'   downloadButton("export", "Download Report"),
#'   plotOutput('plot')
#' )
#' 
#' srv <- function(input, output, session) {
#'   output$plot <- renderPlot({
#'     plot(x = mtcars[[input$x]],
#'          y = mtcars[[input$y]])
#'   })
#'   output$export <- downloadHandler(
#'     filename = 'MyReport.pdf',
#'     content = function(file) report_downloader(
#'       file,
#'       'MyReport.Rmd',
#'       generate_static_code(srv)
#'     )
#'   )
#' }
#' 
#' shinyApp(ui, srv)
#' }
#' 
#' @importFrom shiny withProgress
#' @importFrom rmarkdown render
#' @export
#' 
report_downloader <- function(file, text, format = 'pdf_document', 
    output_options = NULL) {

  valid_format <- format %in% getNamespaceExports('rmarkdown')
  
  # clean params
  if (valid_format) { 
    tmp_filepath <- file.path(
      dirname(file), 
      sprintf('%s.knit_expand.rmd', gsub("\\..*", "", basename(file))))
    output_options <- output_options[names(output_options) %in%
      names(formals(getExportedValue('rmarkdown', format)))]
  } else {
    tmp_filepath <- file
    output_options <- NULL
  }
  
  # show progress message and generate plot
  shiny::withProgress(
    message = 'Building Code & Running Report',
    detail = "this may take a bit of time ...",
    value = NULL, {

    # write out template to temp file that rmarkdown::render can use
    writeChar(text, tmp_filepath, eos = NULL)
      
    # render expanded temp file out to target filetype
    if (!valid_format) return() # don't need to render
    
    output_file <- rmarkdown::render(
      input = tmp_filepath,
      output_format = format,
      output_file = file,
      output_options = output_options,
      envir = new.env(parent = globalenv()))

    # ensure filename matches expected download handler filename
    file.rename(output_file, file)

  })
}



#' Fill in a template document with current code
#' 
#' Uses knitr's knit_expand to expand whisker elements. Always fills in a
#' whiskered parameter ".code" with the stateful server code necessary to
#' generate outputs.
#' 
#' @param template a whiskered ("{{ ... }}") knitr template
#' @param server the server function of the caller
#' @param ... additional list to fill in to knitr template
#' @param dots additional list to fill in to knitr template
#' @param envir environment in which code should be generated
#'
#' @importFrom knitr knit_expand
#' @importFrom utils modifyList
#' @importFrom stats setNames
#' @export
report_from_template <- function(template, server, ..., dots = list(),
    envir = parent.frame()) {
  
  dots <- c(list(...), as.list(dots))
  
  outputs <- intersect(
    shiny_output_names(body(server)), 
    knitr_expand_content_list(template))
  
  code <- generate_static_code(server, dots = outputs, 
      flatten_outputs = FALSE, envir = envir)
  
  # create calls to outputs to replace output strings in template
  # such that after knit_expand, `{{plot}}` ==> `output$plot()`
  outputs <- stats::setNames(sprintf('output$%s()', outputs), outputs)
  
  # create a 'hidden' variable '.code' which can be used in templates to load
  # append new variables to 'outputs' for passing to knit_expand
  code <- paste0(code[-1], collapse = '\n')
  outputs <- utils::modifyList(append(outputs, list(`.code` = code)), dots)
  
  # expand {{ ... }}'s
  do.call(knitr::knit_expand, c(template, outputs))
}

