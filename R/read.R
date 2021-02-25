#' @title Read captcha files
#'
#' @description Given the paths to one or more files, reads and converts
#' them into a `captcha` list that can be used for classification or
#' decryption. If `ans_in_path = TRUE`, will take the answer for the
#' captchas from their filenames and get them ready for modeling.
#'
#' @param files Paths to one or more captcha images
#' @param ans_in_path Whether or not the answers to the captchas are already
#' in the paths to the files (separated by and underscore in the filename)
#'
#' @return A list of captcha objects
#'
#' @export
read_captcha <- function(files, ans_in_path = FALSE) {

  imgs <- magick::image_read(files)
  labs <- NULL
  if (ans_in_path) {
    labs <- get_labels(files)
  }

  # Iterate over files
  out <- list(img = imgs, lab = labs, path = files)

  class(out) <- c("captcha")
  return(out)
}

get_labels <- function(files) {
  files %>%
    basename() %>%
    stringr::str_extract("(?<=_)[0-9a-zA-Z]+")
}

# calcular_y <- function(x) {
#   x %>%
#     basename() %>%
#     stringr::str_extract("(?<=_)[0-9a-zA-Z]+") %>%
#     purrr::map(stringr::str_split, "") %>%
#     purrr::map(~torch::torch_tensor(as.integer(.x[[1]]))) %>%
#     # TODO generalizar esse 9 para algo calculado
#     purrr::map(torch::nnf_one_hot, 9) %>%
#     torch:::torch_stack()
# }
#
#
# calcular_x <- function(x, dims = c(32L, 192L)) {
#   x %>%
#     purrr::map(torchvision::base_loader) %>%
#     purrr::map(torchvision::transform_to_tensor) %>%
#     purrr::map(torchvision::transform_rgb_to_grayscale) %>%
#     torch:::torch_stack() %>%
#     torchvision::transform_resize(dims)
# }
#

