#' Function to solve Captchas
#'
#' @param files files to read. Can be a character vector or an object of class `captcha`.
#' @param model model of class `luz_module_fitted`
#'
#' @name decrypt
#' @export
decrypt <- function(files, model) {
  UseMethod("decrypt")
}

#' @export
decrypt.captcha <- function(files, model) {
  decrypt.default(captcha$path, model)
}

#' @export
decrypt.default <- function(files, model) {
  as.character(purrr::map_chr(files, decrypt_, model))
}

decrypt_ <- function(file, mm) {
  mm$model$eval()
  transformed <- mm$model$transform(file)
  ind <- mm$model(transformed) |>
    torch::torch_argmax(3) |>
    as.matrix()
  apply(ind, 1, function(x) paste(mm$model$vocab[x], collapse = ""))
}
