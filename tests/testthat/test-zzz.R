test_that("test onload", {

  options(captcha.print.rows = "a")
  options(captcha.print.cols = "a")
  options(captcha.print.height = "a")

  captcha:::.onLoad()

  expect_type(getOption("captcha.print.rows"), "double")
  expect_type(getOption("captcha.print.cols"), "double")
  expect_type(getOption("captcha.print.height"), "double")

})

test_that("ghostscript works", {

  has_ghostscript <- magick::magick_config()$ghostscript

  if (has_ghostscript) {
    expect_true(check_magick_ghostscript())
  }
  expect_error(check_magick_ghostscript(TRUE, TRUE))
  expect_message(expect_false(
    check_magick_ghostscript(FALSE, TRUE)
  ), "ImageMagick")

})
