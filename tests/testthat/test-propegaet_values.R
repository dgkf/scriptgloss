context("propegate_values")

test_that("propegating provided values works.", {
  expect_equal({
    scriptgloss:::propegate_values(quote({
      f <- function() {
        print(paste(input$a, input$b, other_var))
      }
    }), input = list(
      a = c(1, 2, 3, 4),
      b = "b"))
  }, {
    quote({
      f <- function() {
        print(paste(c(1, 2, 3, 4), "b", other_var))
      }
    })
  })
})

test_that("propegating from parent frame works.", {
  expect_equal({
    input = list(
      a = c(1, 2, 3, 4),
      b = "b")
    
    envir <- environment()
    
    scriptgloss:::propegate_values(quote({
      f <- function() {
        print(paste(input$a, input$b, other_var))
      }
    }), envir = envir)
  }, {
    quote({
      f <- function() {
        print(paste(c(1, 2, 3, 4), "b", other_var))
      }
    })
  })
})

test_that("A missing list value is assigned NULL.", {
  expect_equal({
    scriptgloss:::propegate_values(quote({
      f <- function() {
        print(input$c + other_var)
      }
    }), input = list(
      a = c(1, 2, 3, 4),
      b = "b"))
  }, {
    quote({
      f <- function() {
        print(NULL + other_var)
      }
    })
  })
})

test_that("Variables assigned inside code block don't get overwritten.", {
  expect_equal({
    scriptgloss:::propegate_values(quote({
      f <- function() {
        input <- 3
        print(input)
      }
    }), input = 1)
  }, {
    quote({
      f <- function() {
        input <- 3
        print(input)
      }
    })
  }) 
})

test_that("Variables assigned inside code block, in another scope, don't get overwritten.", {
  expect_equal({
    scriptgloss:::propegate_values(quote({
      input <- 3
      f <- function() {
        print(input)
      }
    }), input = 1)
  }, {
    quote({
      input <- 3
      f <- function() {
        print(input)
      }
    })
  }) 
})