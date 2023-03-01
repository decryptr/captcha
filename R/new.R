#' Create a new project to solve a new Captcha
#'
#' This function creates a new project with a template implemented inside
#' the captcha package. It creates a new folder containing helpers to
#' download, annotate, fit a custom model and share the results.
#'
#' @param path A path. If it exists, it is used. If it does not exist,
#'   it is created, provided that the parent path exists.
#' @param rstudio Use RStudio? Defaults to `TRUE`.
#' @param open Open new RStudio project? Defaults to `rstudio`.
#'
#' @details
#'
#' After creating a new project, via the new_captcha() command or via the
#' RStudio interface, a new window opens. The project contains four files:
#'
#' * 01_download.R: Contains some code to help writing functions that
#'   download Captchas in a real scenario. In practice, the download
#'   functions need to be adapted because the websites are organized
#'   in very different ways.
#' * 02_annotate.R: Contains a template for manual annotation of Captchas.
#'   Manual annotation can either be performed using the interface created
#'   by the captcha package or externally. The annotated files are stored
#'   in the img folder.
#' * 03_model.R: Contains a template for modeling, allowing complete
#'   customization of the fitting procedure. The script contains commands
#'   to load the data, specify the model, fit the model and save the
#'   fitted model.
#' * 04_share.R: Contains operations to create a git repository of the
#'   solution and make the fitted model available. The model can be loaded
#'   afterwards using the captcha_load_model() function, without the need
#'   to copy files locally.
#'
#' For more details, see `vignette("advanced")`.
#'
#' @return Single logical value indicating if current session is modified.
#'
#' @export
new_captcha <- function(path, rstudio = TRUE, open = rstudio) {
  template <- system.file("template", package = "captcha")
  fs::dir_copy(template, path)
  usethis::proj_set(path, TRUE)
  if (rstudio) usethis::use_rstudio()
  if (open) usethis::proj_activate(path)
}

# used in RStudio GUI
new_captcha_gui <- function(path, ...) {
  new_captcha(path)
}
