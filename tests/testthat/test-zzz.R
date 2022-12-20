test_that("test onload", {

  options(captcha.print.rows = "a")
  options(captcha.print.cols = "a")
  options(captcha.print.height = "a")

  captcha:::.onLoad()

  expect_type(getOption("captcha.print.rows"), "double")
  expect_type(getOption("captcha.print.cols"), "double")
  expect_type(getOption("captcha.print.height"), "double")

})
