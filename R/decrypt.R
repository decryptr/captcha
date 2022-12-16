#' Function to solve Captchas
#'
#' @param files files to read
#' @param model model of class `luz_module_fitted`
#'
#' @export
decrypt <- function(files, model) {
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
