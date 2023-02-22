test_that("new captcha works", {

  skip_if_knitr()
  tmp_dir <- withr::local_tempdir()

  expect_message(new_captcha(tmp_dir))

  files <- dir(paste0(tmp_dir, "/template"))
  files_ref <- c("01_download.R", "02_annotate.R", "03_model.R", "04_deploy.R")
  expect_true(all(files_ref %in% files))
})

test_that("new captcha GUI works", {

  skip_if_knitr()
  skip_on_ci()

  # same as new_captcha()
  tmp_dir <- withr::local_tempdir()

  expect_message(new_captcha_gui(tmp_dir))

  files <- dir(paste0(tmp_dir, "/template"))
  files_ref <- c("01_download.R", "02_annotate.R", "03_model.R", "04_deploy.R")
  expect_true(all(files_ref %in% files))
})
