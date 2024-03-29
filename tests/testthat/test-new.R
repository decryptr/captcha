test_that("new captcha works", {

  tmp_dir <- withr::local_tempdir()

  expect_message(new_captcha(tmp_dir, FALSE, FALSE))

  files <- dir(paste0(tmp_dir, "/template"))
  files_ref <- c("01_download.R", "02_annotate.R", "03_model.R", "04_deploy.R")
  expect_true(all(files_ref %in% files))
})

test_that("new captcha GUI works", {

  # same as new_captcha()
  tmp_dir <- withr::local_tempdir()

  expect_message(new_captcha_gui(tmp_dir, FALSE, FALSE))

  files <- dir(paste0(tmp_dir, "/template"))
  files_ref <- c("01_download.R", "02_annotate.R", "03_model.R", "04_deploy.R")
  expect_true(all(files_ref %in% files))
})
