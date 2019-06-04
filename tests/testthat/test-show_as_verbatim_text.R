context("show_as_verbatim_text")

test_that("an assortment of types match verbatim text output", {
  app_path <- scriptgloss:::shinytest_path("shinytest-show_as_verbatim_text")
  app <- shinytest::ShinyDriver$new(app_path)
    
  choices = list(
    list = list(1, 2, 3, 4, 5),
    mtcars = mtcars,
    character_vector = c("lorem", "ipsum", "other", "text"))
  
  app$setInputs(in_item = names(choices[1]))
  app$waitFor("out_text")
  expect_equal({
    app$getValue("out_text")
  }, {
    scriptgloss:::show_as_verbatim_text(choices[[1]])
  })
  
  app$setInputs(in_item = names(choices[2]))
  app$waitFor("out_text")
  expect_equal({
    app$getValue("out_text")
  }, {
    scriptgloss:::show_as_verbatim_text(choices[[2]])
  })
    
  app$setInputs(in_item = names(choices[3]))
  app$waitFor("out_text")
  expect_equal({
    app$getValue("out_text")
  }, {
    scriptgloss:::show_as_verbatim_text(choices[[3]])
  })
  
  app$stop()
})
