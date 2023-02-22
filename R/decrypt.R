#' Function to solve Captchas
#'
#' Returns a label for an image using a fitted model. The image can be either a
#' character vector (of length one or more) or an object of class `captcha`.
#'
#' @param files files to read. Can be either a character vector
#'   or an object of class `captcha`.
#' @param model model of class `luz_module_fitted`
#'
#' @return character vector of the predicted labels.
#'
#' @examples
#'
#' captcha_file <- system.file(
#'   "examples/captcha/cadesp.jpg",
#'   package = "captcha"
#' )
#'
#' cap <- read_captcha(captcha_file)
#'
#' if (interactive()) {
#'   plot(cap)
#' }
#'
#' # the code below uses access to the internet. If you want to run locally,
#' # download the model object from the releases site.
#' if (interactive()) {
#'   model <- captcha_load_model("cadesp")
#'   decrypt(cap, model_rfb)
#' }
#'
#' @name decrypt
#' @export
decrypt <- function(files, model) {
  UseMethod("decrypt")
}

#' @export
decrypt.captcha <- function(files, model) {
  decrypt.default(files$path, model)
}

#' @export
decrypt.default <- function(files, model) {
  as.character(purrr::map_chr(files, decrypt_, model))
}

decrypt_ <- function(file, mm) {

  stopifnot(class(mm) %in% "luz_module_fitted")

  mm$model$eval()
  transformed <- mm$model$transform(file)
  ind <- mm$model(transformed) |>
    torch::torch_argmax(3) |>
    as.matrix()
  apply(ind, 1, function(x) paste(mm$model$vocab[x], collapse = ""))
}
