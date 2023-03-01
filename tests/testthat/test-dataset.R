# transform labels --------------------------------------------------------

test_that("one hot encoding works", {

  vocab <- letters
  all_letters <- c("a", "b", "c", "c")
  res <- captcha_transform_label(all_letters, vocab)

  expect_s3_class(res, "torch_tensor")
  expect_equal(dim(res), c(length(all_letters), 1, length(unique(vocab))))
  expect_equal(as.numeric(sum(res)), length(all_letters))

  all_letters <- list(
    list(c("a", "b", "c", "d")),
    list(c("e", "f", "g", "h")),
    list(c("i", "j", "k", "l"))
  )
  res <- captcha_transform_label(all_letters, vocab)

  expect_s3_class(res, "torch_tensor")
  expect_equal(
    dim(res),
    c(
      length(all_letters),
      length(all_letters[[1]][[1]]), length(unique(vocab))
    )
  )
  expect_equal(as.numeric(sum(res)), length(unlist(all_letters)))


})

test_that("one hot encoding doesn't handle multiple lenghts (yet)", {

  vocab <- letters

  all_letters <- list(
    list(c("a", "b", "c", "d", "z")),
    list(c("e", "f", "g", "h")),
    list(c("i", "j", "k", "l"))
  )
  expect_error(
    captcha_transform_label(all_letters, vocab),
    "same length"
  )
})

test_that("one hot vocab can't have duplicates", {
  expect_error(captcha_transform_label("a", c("a", "a")))
})


# transform image ---------------------------------------------------------

test_that("transform image works", {

  # black and white img
  my_dim <- c(20, 20)
  file <- test_path("examples/jucesp.jpg")
  img_bw <- captcha_transform_image(file, input_dim = my_dim)

  # coloured image
  file <- test_path("examples/tjmg.jpeg")
  img_col <- captcha_transform_image(file, input_dim = my_dim)

  expect_equal(dim(img_bw), c(1, 3, my_dim))
  expect_equal(dim(img_col), c(1, 3, my_dim))

})

# dataset -----------------------------------------------------------------

test_that("captcha dataset works", {

  path <- test_path("examples/dataset")
  files <- fs::dir_ls(path)
  n_files <- length(files)
  m <- capture_messages(ds <- captcha_dataset(path))

  expect_match(m, "Processing", all = FALSE)
  expect_match(m, "Done", all = FALSE)
  expect_s3_class(ds, "my_captcha")
  expect_s3_class(ds, "dataset")
  expect_s3_class(ds, "R6")
  expect_equal(length(ds), n_files)
  expect_equal(ds$files, files)

  expect_equal(
    dim(ds$data),
    c(n_files, 3, 32, 192)
  )

  lst <- files |>
    stringr::str_extract("(?<=_)[0-9a-z]+") |>
    stringr::str_split("")

  len <- length(lst[[1]])
  vocab <- unique(unlist(lst))

  expect_equal(
    dim(ds$target),
    c(n_files, len, length(vocab))
  )

  expect_equal(length(ds[1:2]), 2)

})
