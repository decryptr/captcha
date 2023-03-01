# file path ---------------------------------------------------------------

test_that("creates folder and annotated file", {

  f_captcha <- test_path("examples/tjpe.png")

  # create local dir and delete it
  tmp_dir <- withr::local_tempdir()
  fs::dir_delete(tmp_dir)

  # now we annotate and verify if the directory and file exist
  result <- captcha_annotate(f_captcha, labels = "lala", path = tmp_dir)

  expect_true(fs::dir_exists(tmp_dir))
  expect_true(fs::file_exists(result))
  expect_match(result, "_lala")
})

test_that("prompt", {

  f_captcha <- test_path("examples/tjpe.png")

  # create local dir and delete it
  tmp_dir <- withr::local_tempdir()
  fs::dir_delete(tmp_dir)

  f <- file()
  write("lala", f)

  # now we annotate and verify if the directory and file exist
  result <- captcha_annotate(f_captcha, labels = f, path = tmp_dir)

  expect_true(fs::dir_exists(tmp_dir))
  expect_true(fs::file_exists(result))
  expect_match(result, "_lala")
})

test_that("vector of labels works", {
  f_captcha <- rep(test_path("examples/tjpe.png"), 2)

  # create local dir
  tmp_dir <- withr::local_tempdir()

  # now we annotate and verify if the length of the output is correct
  result <- captcha_annotate(
    f_captcha,
    labels = c("lala", "lala"),
    path = tmp_dir
  )
  expect_length(result, 2)
})

test_that("wrong length throws error", {

  f_captcha <- rep(test_path("examples/tjpe.png"), 2)

  # create local dir
  tmp_dir <- withr::local_tempdir()

  expect_error(captcha_annotate(
    f_captcha,
    labels = c("lala"), # wrong length
    path = tmp_dir
  ))
})

test_that("removes old files", {
  f_captcha <- test_path("examples/tjpe.png")

  # create local dir
  tmp_dir <- withr::local_tempdir()
  # move original file from the package to tempdir
  new_f_captcha <- fs::file_copy(f_captcha, tmp_dir)

  # now we annotate and verify if the old file has been deleted
  result <- captcha_annotate(
    new_f_captcha,
    labels = "lala",
    path = tmp_dir,
    rm_old = TRUE
  )
  expect_false(fs::file_exists(new_f_captcha))
})

# captcha class -----------------------------------------------------------

test_that("creates folder and annotated file, captcha class", {
  f_captcha <- test_path("examples/tjpe.png")
  captcha <- read_captcha(f_captcha)

  # create local dir and delete it
  tmp_dir <- withr::local_tempdir()
  fs::dir_delete(tmp_dir)

  # now we annotate and verify if the directory and file exist
  result <- captcha_annotate(captcha, labels = "lala", path = tmp_dir)

  expect_true(fs::dir_exists(tmp_dir))
  expect_true(fs::file_exists(result))
  expect_match(result, "_lala")
})

test_that("vector of labels works, captcha class", {
  f_captcha <- rep(test_path("examples/tjpe.png"), 2)
  captcha <- read_captcha(f_captcha)

  # create local dir
  tmp_dir <- withr::local_tempdir()

  # now we annotate and verify if the length of the output is correct
  result <- captcha_annotate(
    captcha,
    labels = c("lala", "lala"),
    path = tmp_dir
  )
  expect_length(result, 2)
})

test_that("wrong length throws error, captcha class", {

  f_captcha <- rep(test_path("examples/tjpe.png"), 2)
  captcha <- read_captcha(f_captcha)

  # create local dir
  tmp_dir <- withr::local_tempdir()

  expect_error(captcha_annotate(
    captcha,
    labels = c("lala"), # wrong length
    path = tmp_dir
  ))
})

test_that("removes old files, captcha class", {
  f_captcha <- test_path("examples/tjpe.png")

  # create local dir
  tmp_dir <- withr::local_tempdir()
  # move original file from the package to tempdir
  new_f_captcha <- fs::file_copy(f_captcha, tmp_dir)

  captcha <- read_captcha(new_f_captcha)

  # now we annotate and verify if the old file has been deleted
  result <- captcha_annotate(
    captcha,
    labels = "lala",
    path = tmp_dir,
    rm_old = TRUE
  )
  expect_false(fs::file_exists(new_f_captcha))
})

