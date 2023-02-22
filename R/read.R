#' @title Read captcha files
#'
#' @description Given the paths to one or more files, reads and converts
#' them into a `captcha` list that can be used for modeling or
#' prediction. If `lab_in_path=TRUE`, will take the label for the
#' Captchas from their filenames using `_` as the separator.
#'
#' @param files Paths to one or more captcha images
#' @param lab_in_path Whether or not the labels to the captchas are already
#'   in the paths to the files (separated by and underscore in the filename)
#'
#' @details
#' The `read_captcha()` function reads a character vector of image files and
#' stores them in memory. Behind the scenes, the function uses the
#' [`{magick}`](https://docs.ropensci.org/magick/) package to deal with
#' the types of files that may appear (JPEG, PNG, among others).
#'
#' @return A list of class `captcha`. The `captcha` object is a list with
#' three elements: `$img`, which contains the image read from the `{magick}`
#' package; `$lab`, which contains the image label (by default, `NULL`); and
#' `$path`, which contains the path of the image.
#'
#' @examples
#'
#' captcha_file <- system.file("examples/captcha/esaj.png", package = "captcha")
#' cap <- read_captcha(captcha_file)
#' cap
#'
#' @export
read_captcha <- function(files, lab_in_path = FALSE) {

  imgs <- magick::image_read(files)
  labs <- NULL
  if (lab_in_path) {
    labs <- get_labels(files)
    if (any(is.na(labs))) {
      warning("The labels for some files were not found.")
    }
  }

  # Iterate over files
  out <- list(img = imgs, lab = labs, path = files)

  class(out) <- c("captcha")

  out
}

get_labels <- function(files) {
  files |>
    basename() |>
    tools::file_path_sans_ext() |>
    stringr::str_extract("(?<=_)[0-9a-zA-Z]+($|[._])")
}

