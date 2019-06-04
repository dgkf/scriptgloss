context("reactiveVal get stripped from shiny code")

test_that("reactiveVal calls get converted to nullary functions", {
  app_path <- scriptgloss:::shinytest_path("shinytest-reactiveVal")
  app <- shinytest::ShinyDriver$new(app_path)
    
  app$setInputs(increment_rv = "click")
  app$setInputs(increment_rv = "click")
  app$setInputs(increment_rv = "click")
  
  expect_equal({
    app$getValue("reactiveVal")
  }, {
    capture.output(print(3))
  })
  
  expect_equal({
    app$getValue("reactiveVal_code")
  }, {
    "rv <- function() 3\nrv()"
  })
  
  app$stop()
})
