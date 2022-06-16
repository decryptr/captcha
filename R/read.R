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
  files |>
    basename() |>
    stringr::str_extract("(?<=_)[0-9a-zA-Z]+")
}

