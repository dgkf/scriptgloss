% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{check_shiny}
\alias{check_shiny}
\title{Verify that shiny can be used in current scope}
\usage{
check_shiny(calling_f = as.list(sys.call(-1L))[[1]],
  ignore = getOption("scriptgloss.testmode"))
}
\arguments{
\item{calling_f}{a function name which triggered the check_shiny call.
defaults to the name of the calling function.}

\item{ignore}{logical indicating whether to return TRUE even if shiny is not
currently running (generally only used for testing purposes.)}
}
\value{
logical indicating whether shiny namespace is available
}
\description{
Verify that shiny can be used in current scope
}
