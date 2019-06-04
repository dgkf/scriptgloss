context("reactiveValues get stripped from shiny code")

test_that("reactiveValues calls get converted to static lists", {
  app_path <- scriptgloss:::shinytest_path("shinytest-reactiveValues")
  app <- shinytest::ShinyDriver$new(app_path)
    
  app$setInputs(increment_rvs = "click")
  app$setInputs(increment_rvs = "click")
  app$setInputs(increment_rvs = "click")
  
  expect_equal({
    app$getValue("reactiveValues")
  }, {
    trimws(paste0(capture.output(print(
      list(a = 4, b = c('a', 'b', 'c', 'd'), c = TRUE)
    )), collapse = "\n"))
  })
  
  expect_equal({
    app$getValue("reactiveValues_code")
  }, {
    "list(a = 4, b = c(\"a\", \"b\", \"c\", \"d\"), c = TRUE)"
  })
  
  app$stop()
})
