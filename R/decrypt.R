#' Funcao de resolver o captcha
#'
#' @param file arquivo para ler
#' @param mm modelo
#'
#' @export
decrypt <- function(file, mm) {
  mm$model$eval()
  transformed <- mm$model$transform(file)
  ind <- mm$model(transformed) %>%
    torch::torch_argmax(3) %>%
    as.matrix()
  apply(ind, 1, function(x) paste(mm$model$vocab[x], collapse = ""))
}

