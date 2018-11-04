context("append_declaration")

test_that("Variables get effectively initialized", {
  options(scriptgloss.testmode = TRUE)
  
  code <- quote({
    a <- tolower(a)
    b <- b[[1]]
    c <- tolower(c)
    print(sprintf('this is %s %dst %s', a, b, c))
  })
  
  expect_equal({
    scriptgloss:::append_declaration(as.list(code)[-1],
      a = 'my',
      b = list(1,2,3),
      c = 'example', after = 0)
  }, {
    list(
      quote(a <- "my"), 
      quote(b <- list(1, 2, 3)), 
      quote(c <- "example"), 
      quote(a <- tolower(a)), 
      quote(b <- b[[1]]), 
      quote(c <- tolower(c)), 
      quote(print(sprintf("this is %s %dst %s", a, b, c))))
  })
  
  options(scriptgloss.testmode = FALSE)
})

test_that("shiny::reactiveValues get frozen and initialized as list", {
  app_path <- scriptgloss:::shinytest_path("shinytest-append_declaration")
  app <- shinytest::ShinyDriver$new(app_path)
  app$setInputs(a = "mpg", b = "wt")
  
  expect_equal({
    app$getValue("code")
  }, {
    scriptgloss:::show_as_verbatim_text(list(
      quote(input <- list(a = "mpg", b = "wt")),
      quote(print("Here"))
    ))
  })
  
  app$stop()
})