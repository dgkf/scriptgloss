context("getInitializationCall")

test_that("getInitializationCall(<list>)", {
  # safeguard against deprecations as observed in Issue #9
  expect_equal({
    scriptgloss::getInitializationCode(list(a = 1, b = 2))
  }, {
    quote(list(a = 1, b = 2))
  })
})