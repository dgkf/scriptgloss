% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/code-value-propegation.R
\name{propegate_values}
\alias{propegate_values}
\title{Partially evaluate a code block with given variable values}
\usage{
propegate_values(code, ..., dots = list(), envir = NULL)
}
\arguments{
\item{code}{the code block to use as the basis for evaluation}

\item{...}{named variables and values to be evaluated within the code}

\item{dots}{optionally specify additional ellipses names as list}

\item{envir}{the environment in which subtrees should be evaluated. If
missing (the default) an environment as a direct child of the base
environment is created with only declarations of the ellipses arguments and
used for evaluation of the tree.}
}
\value{
the input code block with specified variables partially applied
}
\description{
Partially evaluate a code block with given variable values
}
\examples{
codeblock <- quote({
  # inserting values into a code block
  my_function_1 <- function() {
    print(paste(input$a, input$b, other_var))
  }

  my_function_2 <- function() {
    print(input$a)
    print(input$b)
  }
  
  # example of behavior when a value is not present
  my_function_2 <- function() {
    print(input$c + other_var)
  }
  
  # example of variable getting declared in a local frame
  my_function_3 <- function() {
    input <- 3
    print(input)
  }
  
  # example of a variable getting declared in a parent frame
  input <- 4
  my_function_4 <- function() {
    print(input)
  }
})


codeblock <- quote({
  output$test <- function() { print("a") }
  output$test()
})

scriptgloss:::propegate_values(
    codeblock, 
    input = list(
      a = list(1, 2, 3, 4), 
      b = "b"))

}
