test_that("load local model", {

  file <- test_path("examples/tjpe.png")

  # load local model works
  model <- captcha_load_model(test_path("examples/model.pt"))
  lab <- decrypt(file, model)

  expect_s3_class(model, "luz_module_fitted")
  expect_type(lab, "character")

})

test_that("load remote model", {

  file <- test_path("examples/tjpe.png")

  # model from captcha package
  model_pkg <- captcha_load_model("rfb")

  # model from contributed package
  model_contrib <- captcha_load_model("r_captcha", "jtrecenti/r_captcha")

  # does not exist
  expect_warning(captcha_load_model("rfb", "jtrecenti/r_captcha"))

  lab_pkg <- decrypt(file, model_pkg)
  lab_contrib <- decrypt(file, model_contrib)

  expect_s3_class(model_pkg, "luz_module_fitted")
  expect_s3_class(model_contrib, "luz_module_fitted")
  expect_type(lab_pkg, "character")
  expect_type(lab_contrib, "character")

})

test_that("available models", {

  models <- available_models()

  remote_models <- piggyback::pb_list(
    repo = "decryptr/captcha",
    tag = "captcha_model"
  )

  expect_true(all(models %in% tools::file_path_sans_ext(remote_models$file_name)))

})
