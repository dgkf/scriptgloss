context("is_shiny_output_assignment")

test_that("is_shiny_output_assignment detects if a top level statement is an output assignment", {
  expect_true({
    scriptgloss:::is_shiny_output_assignment(quote(output$test <- renderPlot({})))
  })
  
  expect_false({
    scriptgloss:::is_shiny_output_assignment(quote(print("not output")))
  })
})