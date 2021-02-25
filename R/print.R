#' Print information about a captcha
#'
#' @param x Captcha object read with [read_captcha()]
#' @param ... -
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

length.captcha <- function(captcha) {
  length(captcha$img)
}

#' Plot a captcha
#'
#' @param x Captcha object read with [read_captcha()]
#' @param y Not used
#' @param ... Other arguments passed on to [graphics::plot()]
#'
#' @export
plot.captcha <- function(x, y, ...) {

  # N <- getOption("captcha.print.max")
  # if (length(x$img) > N) {
  #   usethis::ui_todo(stringr::str_glue(
  #     "Too many images, printing first {N}. ",
  #     "To override, run options('captcha.print.max' = N_MAX)"
  #   ))
  # }

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
      boxcolor = "white"
    )
  }

  op <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = rep(0, 4L))
  if (length(img) > 1) {
    img %>%
      magick::image_border(color = "white", geometry = "5x5") %>%
      magick::image_montage(
        tile = stringr::str_glue("{columns}x{rows}"),
        geometry = stringr::str_glue(
          "{getOption('captcha.print.height')}x0+0+0"
        )
      ) %>%
      graphics::plot()
  } else {
    graphics::plot(img)
  }
  graphics::par(op)
}
