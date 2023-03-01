test_that("print captcha as expected", {

  skip_if_knitr()

  f_captcha <- test_path("examples/tjpe.png")
  captcha <- read_captcha(f_captcha)

  expect_snapshot(print(captcha))
})


test_that("plot captcha as expected", {

  # from the testthat documentation

  skip_if_knitr()

  save_png <- function(code, width = 400, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    on.exit(dev.off())
    code
    path
  }

  f_captcha <- test_path("examples/tjpe.png")
  captcha <- read_captcha(f_captcha)

  expect_snapshot_file(save_png(plot.captcha(captcha)), "plot.png")
})

test_that("plot captcha as expected, big images", {

  skip_if_knitr()

  # from the testthat documentation
  save_png <- function(code, width = 400, height = 400) {
    path <- tempfile(fileext = ".png")
    png(path, width = width, height = height)
    on.exit(dev.off())
    code
    path
  }

  f_captcha <- rep(test_path("examples/tjpe.png"), 110)

  captcha <- read_captcha(f_captcha)

  expect_message(
    expect_snapshot_file(save_png(plot.captcha(captcha)), "plot_many.png"),
    "Too many"
  )
})

test_that("plot captcha with annotation", {

  skip_if_knitr()

  if (check_magick_ghostscript(error = FALSE)) {
    # from the testthat documentation
    save_png <- function(code, width = 400, height = 400) {
      path <- tempfile(fileext = ".png")
      png(path, width = width, height = height)
      on.exit(dev.off())
      code
      path
    }

    f_captcha <- test_path("examples/tjpe_4wba3.png")
    captcha <- read_captcha(f_captcha)

    expect_snapshot_file(save_png(plot.captcha(captcha)), "plot_annotation.png")
  } else {
    expect_error(check_magick_ghostscript())
  }

})

test_that("subset and length works", {

  f_captcha <- rep(test_path("examples/tjpe.png"), 10)
  captcha <- read_captcha(f_captcha)

  captcha_subs <- captcha[1:4]

  expect_length(captcha, 10)
  expect_length(captcha_subs, 4)
  expect_equal(length(captcha), 10)
  expect_equal(length(captcha_subs), 4)



})
