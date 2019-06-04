context("reduce_nested_syntax")

test_that("reducing syntax works as expected", {
  q <- quote({
      # nested {}'s should be unwrapped when it has no barring on output
      {
        {
          a <- 1
        }
      }
      #                                           even in function args
      x <- function(a = 1, b = { z <- a; z + 1 }, c = { z + 1 }) {
        quote({
          a <- 1
          # and within quote declarations
          {
            b <- 2
          }
        })
        a <- 2
      }
    })
  
  # for whatever reason, putting reduce_nested_syntax in the expect_equal call 
  # throws an rlang error... unclear why this happens
  a <- scriptgloss:::reduce_nested_syntax(quote({{{{{}}}}}))
  b <- quote({})
  expect_equal(a, b)
  
  a <- scriptgloss:::reduce_nested_syntax(quote({ f <- function() {{{{{}}}}} }))
  b <- quote(f <- function() {})
  expect_equal(a, b)
  
  a <- scriptgloss:::reduce_nested_syntax(quote({{{{{ a <- 1 }}}}}))
  b <- quote(a <- 1)
  expect_equal(a, b)
  
  a <- scriptgloss:::reduce_nested_syntax(quote({ f <- function() {{{{{ a <- 1 }}}}} }))
  b <- quote(f <- function() a <- 1)
  expect_equal(a, b)
  
  a <- scriptgloss:::reduce_nested_syntax(q)
  b <- quote({
    a <- 1
    x <- function(a = 1, b = { z <- a; z + 1}, c = z + 1) {
      quote({
        a <- 1
        b <- 2
      })
     a <- 2
    }
  })
  expect_equal(a, b)
})