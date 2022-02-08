#' List of currently available captchas
#'
#' @export
available_captchas <- function() {
  c("rfb", "trt2")
}

#' File to torch tensor
#'
#' @param x file path
#'
#' @export
captcha_transform_image <- function(x, input_dim = c(32L, 192L)) {
  x %>%
    purrr::map(torchvision::base_loader) %>%
    purrr::map(torchvision::transform_to_tensor) %>%
    purrr::map(adjust_dimensions) %>%
    purrr::map(torchvision::transform_resize, input_dim) %>%
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
    img[1:3]
  } else {
    torch::torch_stack(list(img[1], img[1], img[1]))
  }
}


#' File to response matrix (tensor)
#'
#' @param all_letters list of tokens for all files
#' @param vocab unique tokens
#'
#' @export
captcha_transform_label <- function(all_letters, vocab) {


  all_letters %>%
    purrr::map(~{
      torch::torch_tensor(as.integer(factor(.x[[1]], levels = vocab)))
    }) %>%
    purrr::map(torch::nnf_one_hot, length(vocab)) %>%
    torch::torch_stack()
}

#' Captcha data URLs
#'
#' @export
captcha_data_url <- function() {
  u_base <- "https://storage.googleapis.com/decryptr/data-raw"
  as.list(stats::setNames(
    stringr::str_glue("{u_base}/{available_captchas()}.zip"),
    available_captchas()
  ))
}

#' Captcha datasets
#'
#' @param root (string): root directory of dataset where `captcha.zip`
#'   exists or will be saved to if download is set to `TRUE`
#' @param captcha (string): name of the captcha, must be one of
#'   [available_captchas()]()
#' @param train (bool, optional): If `TRUE`, the default, creates
#'   dataset from training set.
#' @param transform_image (callable, optional): A function/transform
#'   that takes in an file path and returns an torch tensor prepared
#'   to feed the model.
#' @param transform_label (callable, optional): A function/transform
#'   that takes in the file paths and transform them.
#' @param download (bool, optional): If `TRUE`, downloads the dataset
#'   from the internet and puts it in `root`. If dataset is already
#'   downloaded, it is not downloaded again. Defaults to `FALSE`
#' @param in_memory (bool, optional) If `TRUE`, the default, loads
#'   all the files in memory. If `FALSE`, it exports a data generator
#'   function to read batches from disk.
#' @param augmentation (function, optional) If not `NULL`, applies a
#'   function to augment data with randomized preprocessing layers.
#'
#' @export
captcha_dataset <- torch::dataset(
  name = "my_captcha",
  initialize = function(root, captcha, train = TRUE,
                        transform_image = captcha::captcha_transform_image,
                        transform_label = captcha::captcha_transform_label,
                        download = FALSE, in_memory = TRUE,
                        augmentation = NULL) {

    ## parameter checks
    if (download && missing(captcha)) {
      usethis::ui_stop(c(
        "If download = TRUE, must provide captcha name.",
        "Available names are: {paste(available_captchas(), collapse = ', ')}"
      ))
    }

    ## create directory and assign
    self$path <- root
    fs::dir_create(root)

    ## global variables to use along the class
    self$captcha <- captcha

    ## download file from repository
    if (download) {
      self$download(captcha)
    }

    usethis::ui_info("Processing...")

    ## build dataset
    if (in_memory) {
      files <- fs::dir_ls(root, recurse = TRUE, type = "file")
      self$files <- files

      all_letters <- files %>%
        basename() %>%
        tools::file_path_sans_ext() %>%
        stringr::str_extract("(?<=_)[0-9a-zA-Z]+") %>%
        purrr::map(stringr::str_split, "")

      vocab <- sort(unique(unlist(all_letters)))

      # browser()
      x <- transform_image(files)
      y <- transform_label(all_letters, vocab)
    } else {
      usethis::ui_stop("Not implemented yet.")
    }

    usethis::ui_info("Done!")
    self$data <- x
    self$target <- y
    self$vocab <- vocab
    self$transform <- transform_image
    self$augmentation <- augmentation

  },

  resources = captcha_data_url(),

  # download captcha zip file and unzip it
  download = function(captcha) {
    u <- self$resources[[captcha]]
    dir <- self$path

    ## for testing purposes
    # u <- captcha_data_url()$trt
    # dir <- "~/Downloads/trt"

    ## download
    fs::dir_create(dir)
    filename <- basename(u)
    destpath <- file.path(dir, filename)
    withr::with_options(
      list(timeout = 600),
      utils::download.file(u, destfile = destpath)
    )

    # TODO md5 sum check

    ## unzip and delete original
    zip::unzip(destpath, exdir = dir)
    fs::file_delete(destpath)

  },
  # check if file exists
  check_exists = function() {
    usethis::ui_stop("not implemented")
  },
  # returns a subset of indexed captchas
  .getitem = function(index) {

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
