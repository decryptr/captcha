#' Print information about a captcha
#'
#' This function prints the image as a `magick-image` object on the screen.
#'
#' @param x Captcha object read with [read_captcha()].
#' @param ... not used.
#'
#' @details
#' The `captcha` object is a list with three elements: `$img`, which contains
#' the image read from the `{magick}` package; `$lab`, which contains the
#' image label (by default, `NULL`); and `$path`, which contains the path
#' of the image.
#'
#' The print method gets the `$img` element from this list and prints it.
#'
#' @export
print.captcha <- function(x, ...) {
  old_setup <- getOption("magick.viewer")
  options(magick.viewer = NULL)
  on.exit(options(magick.viewer = old_setup))
  print(x$img)
}

#' @export
"[.captcha" <- function(x, i) {
  stopifnot(is.numeric(i))
  captcha_subset(x, i)
}

captcha_subset <- function(x, index) {
  out <- list(
    img = x$img[index],
    lab = x$lab[index],
    path = x$path[index]
  )
  class(out) <- "captcha"
  out
}

#' @export
length.captcha <- function(x) {
  length(x$img)
}

#' Plot a Captcha
#'
#' This function plots a captcha object on the screen. It is a S3 method
#' for the [graphics::plot()] function.
#'
#' @param x Captcha object read with [read_captcha()]
#' @param y Not used
#' @param ... Other arguments passed on to [graphics::plot()]
#'
#' @details
#'
#' The `plot()` function is a method of
#' [class S3](https://adv-r.hadley.nz/s3.html) from base R.
#' The function facilitates the visualization of Captchas. The function
#' receives a list of images (obtained with the `read_captcha()` function)
#' and displays the Captcha visually.
#'
#' An interesting aspect of the `plot()` function is that it deals with a
#' list of Captchas. It is useful when the goal is to view several Captchas
#' in the image simultaneously. The next image shows an example.
#'
#' By default, the `plot()` function arranges the images into four columns.
#' To change the default, one can modify the options using
#' `options(captcha.print.cols = N)`, where `N` is the desired number of
#' columns. The next image shows an example with two columns.
#'
#' When the list of Captchas is too long, the `plot()` function displays a
#' maximum number of images accompanied by a message. By default, this
#' number is 100, with 25 rows and four columns. One can override the option
#' by combining the `captcha.print.cols=` and `captcha.print.rows=` options.
#'
#' It is possible to create subsets of `captcha` objects using the `[`
#' operator. One can also use the `length()` function to measure the
#' number of images.
#'
#' Finally, when the image has a label, the `plot()` function shows the
#' label on the corner of the image.
#'
#' @export
plot.captcha <- function(x, y, ...) {

  img <- x$img
  lab <- x$lab

  columns <- getOption("captcha.print.cols")
  rows <- ceiling(length(img) / columns)
  max_rows <- getOption("captcha.print.rows")

  if (rows > max_rows) {
    rows <- max_rows
    img <- utils::head(img, rows * columns)
    lab <- utils::head(lab, rows * columns)
    usethis::ui_info(stringr::str_glue(
      "Too many images, printing first {max_rows * columns}. ",
      "To override, run"
    ))
    usethis::ui_todo("options('captcha.print.rows' = MAX_ROWS)")
    usethis::ui_todo("options('captcha.print.cols' = COLUMNS)")
  }

  if (!is.null(lab)) {
    img <- magick::image_annotate(
      img, lab,
      gravity = "northeast",
      color = "black",
      boxcolor = "white"
    )
  }

  op <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = rep(0, 4L))
  if (length(img) > 1) {
    img |>
      magick::image_border(color = "white", geometry = "5x5") |>
      magick::image_montage(
        tile = stringr::str_glue("{columns}x{rows}"),
        geometry = stringr::str_glue(
          "{getOption('captcha.print.height')}x0+0+0"
        )
      ) |>
      graphics::plot()
  } else {
    graphics::plot(img)
  }
  graphics::par(op)
}
