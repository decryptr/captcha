test_that("test onload", {

  options(captcha.print.rows = "a")
  options(captcha.print.cols = "a")
  options(captcha.print.height = "a")

  captcha:::.onLoad()

  expect_type(getOption("captcha.print.rows"), "double")
  expect_type(getOption("captcha.print.cols"), "double")
  expect_type(getOption("captcha.print.height"), "double")

})

test_that("skip if knitr", {

  withr::local_envvar(NOT_ON_KNITR = "")
  expect_condition(skip_if_knitr(), class = "skip")

  withr::local_envvar(NOT_ON_KNITR = "1")
  expect_true(skip_if_knitr())

})
