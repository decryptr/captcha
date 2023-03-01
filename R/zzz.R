utils::globalVariables(c("super"))

.onLoad <- function(libname, pkgname) {
  options(captcha.print.rows = 25)
  options(captcha.print.cols = 4)
  options(captcha.print.height = 150)
}

check_magick_ghostscript <- function(error = TRUE,
                                     test_error_message = FALSE) {
  has_ghostscript <- magick::magick_config()$ghostscript
  if (!has_ghostscript || test_error_message) {
    message <- paste(
      "This package needs ImageMagick with ghostscript enabled",
      "to run captcha_generate() and plot.captcha() functions.",
      "Your ImageMagick installation does not have ghostscript. Please check",
      "the {magick} package documentation for details on how to",
      "install ImageMagick with full features:",
      "https://docs.ropensci.org/magick/#installation.",
      sep = "\n"
    )
    if (error) stop(message) else message(message)

    return(invisible(FALSE))
  }
  invisible(TRUE)
}
