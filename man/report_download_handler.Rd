% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny-report-download-handler.R
\name{report_download_handler}
\alias{report_download_handler}
\title{Export report as template}
\usage{
report_download_handler(template, server, ..., format = "pdf_document",
  filename = paste(Sys.Date(), gsub("\\\\.[^.]+$", "", template), sep = "-"),
  output_options = list(), envir = parent.frame())
}
\arguments{
\item{template}{whisker formatted template rmarkdown file to use as the basis
for report generation}

\item{server}{server function to use for code generation}

\item{...}{additional whisker tags to expand using
\code{\link[knitr]{knit_expand}}}

\item{format}{the format to be passed to \code{\link[rmarkdown]{render}}}

\item{filename}{filename to use for generated file. defaults to base filename
of template file}

\item{output_options}{additional options to be passed to
\code{\link[rmarkdown]{render}}}

\item{envir}{environment in which to construct report downloader callback}
}
\value{
a shiny downloadHandler to manage building and downloading of report
}
\description{
Export report as template
}
\examples{
\dontrun{
## MyReport.Rmd (in working directory)
# ---
# title: "My Report"
# ---
#
# ```{r initialize_code}
# {{.code}}
# ```
#
# # My Plot
#
# ```{r plot}
# {{plot}}
# ```

library(shiny)

ui <- fluidPage(
  selectInput('x', 'x axis', choices = names(mtcars)),
  selectInput('y', 'y axis', choices = names(mtcars)),
  downloadButton("export", "Download Report"),
  plotOutput('plot')
)

srv <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(x = mtcars[[input$x]],
         y = mtcars[[input$y]])
  })
  output$export <- report_download_handler('MyReport.Rmd', srv)
}

shinyApp(ui, srv)
}

}
