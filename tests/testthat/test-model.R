test_that("conv calculation", {

  valores <- 14:1000

  expect_equal(
    calc_dim_conv(valores),
    floor(((((valores-2)/2)-2)/2-2)/2)
  )

})

test_that("model prediction works", {

  n_digits <- 4
  vocab_size <- 10

  model <- net_captcha(
    input_dim = c(64, 64),
    output_ndigits = n_digits,
    output_vocab_size = vocab_size,
    vocab = 0:9,
    transform = captcha_transform_image,
    dropout = c(.1, .1),
    dense_units = 200
  )

  expect_s3_class(model, "CAPTCHA-CNN")
  expect_s3_class(model, "nn_module")

  file <- test_path("examples/tjpe.png")
  x <- captcha_transform_image(file, c(64, 64))

  y <- model(x)

  expect_s3_class(y, "torch_tensor")
  expect_equal(dim(y), c(1, n_digits, vocab_size))

})
