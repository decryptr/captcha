test_that("makes prediction", {
  f_captcha <- test_path("examples/tjpe.png")
  captcha <- read_captcha(f_captcha)

  f_model <- test_path("examples/model.pt")
  model <- luz::luz_load(f_model)

  label1 <- decrypt(f_captcha, model)
  label2 <- decrypt(captcha, model)

  expect_type(label1, "character")
  expect_type(label2, "character")
  expect_equal(label1, label2)

})

test_that("errors", {

  f_captcha <- test_path("examples/tjpe.png")
  captcha <- read_captcha(f_captcha)

  f_model <- test_path("examples/model.pt")
  model <- luz::luz_load(f_model)

  expect_error(label <- decrypt(f_captcha, f_model), "luz_module_fitted")
  expect_error(label <- decrypt(captcha, f_model), "luz_module_fitted")
  expect_error(decrypt(f_captcha))
  expect_error(decrypt(captcha))

})
