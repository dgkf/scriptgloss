context("append_code")

test_that("append_code works two code blocks", {
  code1 <- quote({
    a <- 1
    b <- 2
    e <- 5
  })
  
  code2 <- quote({
    c <- 3
    d <- 4
  })
  
  expect_equal({
    scriptgloss:::append_code(code1, as.list(code2)[-1], after = 3)
  }, {
    list(
      as.name("{"),
      quote(a <- 1),
      quote(b <- 2),
      quote(c <- 3),
      quote(d <- 4),
      quote(e <- 5)
    )
  })
})

test_that("append_code nested expressions retained", {
  code1 <- quote({
    a <- 1
    b <- 2
    e <- 5
  })
  
  code2 <- quote({
    c <- 3
    d <- 4
  })
  
  expect_equal({
    scriptgloss:::append_code(code1, list(code2))
  }, {
    list(
      as.name("{"),
      quote(a <- 1),
      quote(b <- 2),
      quote(e <- 5),
      quote({
        c <- 3
        d <- 4
      })
    )
  })
})