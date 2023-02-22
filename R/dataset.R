#' File to torch tensor
#'
#' This function uses the `torchvision` package to read and transform the
#' image in a torch tensor. The function tries to adjust the dimensions to
#' deal with black and white or coloured images.
#'
#' @param x character vector with the paths to image files.
#' @param input_dim resize image to dimension. Defaults to 32x192, which is
#' a good default for many Captcha applications.
#'
#' @return torch tensor with dimensions `length(x)`x`3`x`input_dim`.
#'
#' @examples
#'
#' captcha_file <- fs::dir_ls(
#'   system.file("examples/captcha/", package = "captcha"
#' ))
#' result <- captcha_transform_image(captcha_file)
#' class(result)
#' dim(result)
#'
#' @export
captcha_transform_image <- function(x, input_dim = c(32L, 192L)) {
  x |>
    purrr::map(torchvision::base_loader) |>
    purrr::map(torchvision::transform_to_tensor) |>
    purrr::map(adjust_dimensions) |>
    purrr::map(torchvision::transform_resize, input_dim) |>
    torch::torch_stack()
}

adjust_dimensions <- function(img) {
  img_adj <- img[1:3]
  if (all(as.numeric(img_adj) == 0) && dim(img)[1] == 4) {
    img_adj <- torch::torch_stack(list(img[4], img[4], img[4]))
  }
  img_adj
}

#' File to response matrix (tensor)
#'
#' This function performs a one-hot encoding of the label, transform a label
#' with `N` letters in a matrix of dimensions `N`x`length(vocab)`. All the
#' labels must have the same length.
#'
#' @param all_letters list of tokens for all files
#' @param vocab unique tokens
#'
#' @return torch tensor with dimensions `length(all_letters)`x`length(vocab)`
#' containing only zeros and ones. All rows sum exactly one.
#'
#' @examples
#' vocab <- letters
#' resp <- captcha_transform_label(c("a","b","c","d","e"), vocab)
#' class(resp)
#' dim(resp)
#'
#' @export
captcha_transform_label <- function(all_letters, vocab) {

  len <- purrr::map_int(all_letters, \(x) length(x[[1]]))
  unq_len <- unique(len)
  if (length(unq_len) > 1) {
    id_1 <- which(len == unq_len[1])[1]
    id_2 <- which(len == unq_len[2])[1]
    usethis::ui_stop(paste0(
      "All Captchas must have the same length.\n",
      "id {id_1} has length {unq_len[1]} and {id_2} has length {unq_len[2]}."
    ))
  }

  all_letters |>
    purrr::map(~{
      torch::torch_tensor(as.integer(factor(.x[[1]], levels = vocab)))
    }) |>
    purrr::map(torch::nnf_one_hot, length(vocab)) |>
    torch::torch_stack()
}

#' Captcha dataset
#'
#' This object implements a dataset using the [torch::dataset()] framework.
#' It loads all the images in torch tensors, as well as the labels.
#'
#' @param root (string): root directory where the files are stored
#' @param transform_image (callable, optional): A function/transform
#'   that takes in an file path and returns an torch tensor prepared
#'   to feed the model. By default, uses the [captcha_transform_image()]
#'   function.
#' @param transform_label (callable, optional): A function/transform
#'   that takes in the file paths and transform them. By default, uses the
#'   [captcha_transform_label()] function.
#' @param augmentation (function, optional) If not `NULL`, applies a
#'   function to augment data with randomized preprocessing layers.
#'
#' This is an object of class `dataset_generator` created using
#' [torch::dataset()] function. It has a `initialize()` method that
#' takes a directory containing the input images,
#' then assigns all the information in-memory with the array data
#' structure for the response variable. It also has a `.getitem()` method that
#' correctly extracts one observation of the dataset in this data
#' structure, and a `.length()` method that correctly calculates the
#' number of Captchas of the dataset.
#'
#' The function calculates the vocabulary based on the identified values in
#' the dataset.
#'
#' @examples
#' annotated_folder <- system.file(
#'   "examples/annotated_captcha",
#'   package = "captcha"
#' )
#'
#' suppressMessages({
#'   ds <- captcha_dataset(annotated_folder)
#' })
#'
#' # gets the first item (the only item in the example)
#' # returns a list with x and y torch tensors.
#' ds$.getitem(1)
#'
#' @export
captcha_dataset <- torch::dataset(
  name = "my_captcha",
  initialize = function(root,
                        transform_image = captcha::captcha_transform_image,
                        transform_label = captcha::captcha_transform_label,
                        augmentation = NULL) {

    ## create directory and assign
    self$path <- root
    fs::dir_create(root)

    usethis::ui_info("Processing...")

    ## build dataset
    files <- fs::dir_ls(root, recurse = TRUE, type = "file")
    self$files <- files

    all_letters <- files |>
      basename() |>
      tools::file_path_sans_ext() |>
      stringr::str_extract("(?<=_)[0-9a-zA-Z]+") |>
      purrr::map(stringr::str_split, "")

    vocab <- sort(unique(unlist(all_letters)))

    # browser()
    x <- transform_image(files)
    y <- transform_label(all_letters, vocab)

    usethis::ui_info("Done!")
    self$data <- x
    self$target <- y
    self$vocab <- vocab
    self$transform <- transform_image
    self$augmentation <- augmentation

  },

  # check if file exists
  check_exists = function() {
    usethis::ui_stop("not implemented")
  },
  # returns a subset of indexed captchas
  .getitem = function(index) {
    # browser()

    x <- self$data[index,..,drop=TRUE]

    if (!is.null(self$augmentation)) {
      x <- self$augmentation(x)
    }

    y <- self$target[index,..]
    return(list(x = x, y = y))
  },
  # number of files
  .length = function() {
    length(self$files)
  },
  # active bindings (retrive or modify)
  active = list(
    classes = function(cl) {
      if (missing(cl)) c(letters, 0:9) else cl
    }
  )
)
