#' Create a new project to solve a custom captcha
#'
#' @param path A path. If it exists, it is used. If it does not exist,
#'   it is created, provided that the parent path exists.
#'
#' @export
new_captcha <- function(path) {
  template <- system.file("template", package = "captcha")
  fs::dir_copy(template, path)
  usethis::proj_set(path, TRUE)
  usethis::use_rstudio()
  usethis::proj_activate(path)
}

# used in RStudio GUI
new_captcha_gui <- function(path, ...) {
  new_captcha(path)
}
