test_that("captcha fit model returns a model", {

  dir <- test_path("examples/dataset")
  model <- captcha_fit_model(dir, epochs = 1, dense_units = 1)
  expect_s3_class(model, "luz_module_fitted")

})

test_that("captcha fit model with validation returns a model", {

  dir <- test_path("examples/dataset")
  model <- captcha_fit_model(dir, dir_valid = dir, epochs = 1, dense_units = 1)
  expect_s3_class(model, "luz_module_fitted")

})
