context("reactiveValuesToList get stripped from shiny code")

test_that("reactiveValuesToList calls get converted to static lists", {
  app_path <- scriptgloss:::shinytest_path("shinytest-reactiveValuesToList")
  app <- shinytest::ShinyDriver$new(app_path)
    
  app$setInputs(increment_rvs = "click")
  app$setInputs(increment_rvs = "click")
  app$setInputs(increment_rvs = "click")
  
  expect_equal({
    app$getValue("reactiveValues")
  }, {
    capture.output(print(4))
  })
  
  expect_equal({
    app$getValue("reactiveValues_code")
  }, {
    paste(lapply(expression(
      x <- list(a = 4, b = c('a', 'b', 'c', 'd'), c = TRUE),
      x[[1]]
    ), capture.output), collapse = "\n")
  })
  
  app$stop()
})
