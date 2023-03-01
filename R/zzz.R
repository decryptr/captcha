utils::globalVariables(c("super"))

.onLoad <- function(libname, pkgname) {
  options(captcha.print.rows = 25)
  options(captcha.print.cols = 4)
  options(captcha.print.height = 150)
}


skip_if_knitr <- function() {
  if (identical(Sys.getenv("NOT_ON_KNITR"), "")) {
    testthat::skip(paste0(
      "This test does not work when run inside knitr.",
      "Set the NOT_ON_KNITR envvar to force this test"
    ))
  }
  invisible(TRUE)
}

check_magick_ghostscript <- function() {
  has_ghostscript <- magick::magick_config()$ghostscript
  if (!has_ghostscript) {
    stop(paste(
      "This package needs ImageMagick with ghostscript enabled",
      "to run captcha_generate() and plot.captcha() functions.",
      "Your ImageMagick installation does not have ghostscript. Please check",
      "the {magick} package documentation for details on how to",
      "install ImageMagick with full features:",
      "https://docs.ropensci.org/magick/#installation.",
      sep = "\n"
    ))
  }
}
