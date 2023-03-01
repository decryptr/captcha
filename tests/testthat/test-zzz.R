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

test_that("ghostscript works", {

  has_ghostscript <- magick::magick_config()$ghostscript

  if (has_ghostscript) {
    expect_true(check_magick_ghostscript())
  } else {
    expect_error(check_magick_ghostscript())
    expect_false(check_magick_ghostscript(error = FALSE))
  }

})
