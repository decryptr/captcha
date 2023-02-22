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
