test_that("captcha class", {

  f_captcha <- test_path("examples/tjpe.png")

  captcha <- read_captcha(f_captcha)

  expect_s3_class(captcha, "captcha")
  expect_named(captcha, c("img", "lab", "path"))
  expect_null(captcha$lab)
  expect_error(read_captcha("idontexist.png"))

})

test_that("lab_in_path", {

  f_captcha <- test_path("examples/tjpe_4wba3.png")

  captcha <- read_captcha(f_captcha, lab_in_path = TRUE)

  expect_s3_class(captcha, "captcha")
  expect_equal(names(captcha), c("img", "lab", "path"))
  expect_type(captcha$lab, "character")

})

test_that("bad label", {

  f_captcha_empty <- test_path("examples/tjpe.png")
  f_captcha_bad <- test_path("examples/tjpe_4wb@3.png")

  expect_warning(
    captcha_empty <- read_captcha(f_captcha_empty, lab_in_path = TRUE)
  )

  expect_warning(
    captcha_bad <- read_captcha(f_captcha_bad, lab_in_path = TRUE)
  )

  expect_true(is.na(captcha_empty$lab))
  expect_true(is.na(captcha_bad$lab))

})
