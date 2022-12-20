#' File to torch tensor
#'
#' @param x file path
#' @param input_dim resize image to dimension
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

to_gray <- function(img) {
  if (dim(img)[1] >= 3) {
    torchvision::transform_rgb_to_grayscale(img)
  } else {
    img[1]
  }
}

adjust_dimensions <- function(img) {
  if (dim(img)[1] >= 3) {
    img_adj <- img[1:3]
    if (all(as.numeric(img_adj) == 0) && dim(img)[1] == 4) {
      img_adj <- torch::torch_stack(list(img[4], img[4], img[4]))
    }
  } else {
    img_adj <- torch::torch_stack(list(img[1], img[1], img[1]))
  }
  img_adj
}


#' File to response matrix (tensor)
#'
#' @param all_letters list of tokens for all files
#' @param vocab unique tokens
#'
#' @export
captcha_transform_label <- function(all_letters, vocab) {

  all_letters |>
    purrr::map(~{
      torch::torch_tensor(as.integer(factor(.x[[1]], levels = vocab)))
    }) |>
    purrr::map(torch::nnf_one_hot, length(vocab)) |>
    torch::torch_stack()
}

#' Captcha datasets
#'
#' @param root (string): root directory of dataset where `captcha.zip`
#'   exists or will be saved to if download is set to `TRUE`
#' @param transform_image (callable, optional): A function/transform
#'   that takes in an file path and returns an torch tensor prepared
#'   to feed the model.
#' @param transform_label (callable, optional): A function/transform
#'   that takes in the file paths and transform them.
#' @param augmentation (function, optional) If not `NULL`, applies a
#'   function to augment data with randomized preprocessing layers.
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
