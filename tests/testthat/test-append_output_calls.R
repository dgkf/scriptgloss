context("append_output_calls")

test_that("append_output_calls works for arbitrary outputs", {
  expect_equal({
    scriptgloss:::append_output_calls(as.list(list(quote(a <- 1))), 
      'plot', 'table', 'text')
  }, {
    list(
      quote(a <- 1),
      quote(output$plot()),
      quote(output$table()),
      quote(output$text())
    )
  })
})