test_that("generate rcaptcha, save", {

  tmp_dir <- withr::local_tempdir()
  if (check_magick_ghostscript(error = FALSE)) {
    # create local dir
    rcaptcha <- captcha_generate(write_disk = TRUE, path = tmp_dir)
    expect_s3_class(rcaptcha, "captcha")
    expect_type(rcaptcha$path, "character")
    expect_type(rcaptcha$lab, "character")
    expect_true(fs::file_exists(rcaptcha$path))
  } else {
    expect_error(captcha_generate(write_disk = TRUE, path = tmp_dir))
  }

})

test_that("generate rcaptcha, don't save", {

  if (check_magick_ghostscript(error = FALSE)) {
    set.seed(1)
    rcaptcha <- captcha_generate(write_disk = FALSE)
    expect_s3_class(rcaptcha, "captcha")
    expect_null(rcaptcha$path)
    expect_type(rcaptcha$lab, "character")
  } else {
    expect_error(captcha_generate(write_disk = FALSE))
  }

})

test_that("random effects work", {

  if (check_magick_ghostscript(error = FALSE)) {
    set.seed(1)
    rcaptcha <- captcha_generate(
      write_disk = FALSE,
      p_implode = 1,
      p_oilpaint = 1,
      p_noise = 1,
      p_lat = 1
    )
    expect_s3_class(rcaptcha, "captcha")
    expect_null(rcaptcha$path)
    expect_type(rcaptcha$lab, "character")
  } else {
    expect_error(captcha_generate(write_disk = FALSE))
  }

})

test_that("number of chars is correct", {

  if (check_magick_ghostscript(error = FALSE)) {
    n_chars <- 1:25
    r_captchas <- purrr::map(
      n_chars,
      \(n) captcha_generate(n_chars = n, p_oilpaint = .4, p_lat = .3)
    )

    for(ii in seq_along(n_chars)) {
      expect_equal(
        stringr::str_length(r_captchas[[ii]]$lab),
        n_chars[ii]
      )
      expect_s3_class(r_captchas[[ii]], "captcha")
      expect_null(r_captchas[[ii]]$path)
    }
  } else {
    expect_error(check_magick_ghostscript())
  }

})
