#' Funcao de resolver o captcha
#'
#' @param file arquivo para ler
#' @param mm modelo
#'
#' @export
decrypt <- function(file, mm) {
  # vocab <- mm$parm$vocab
  # dims <- mm$parm$input_dim
  # mm$to(device = "cpu")
  # mm$eval()
  # ans <- calcular_x(file, dims)$unsqueeze(1) %>%
  #   valid_transforms() %>%
  #   mm() %>%
  #   torch::torch_max(dim = 3) %>%
  #   purrr::pluck(2)
  # paste(
  #   vocab[as.numeric(ans$to(device = "cpu"))],
  #   collapse = ""
  # )
  mm$model$eval()
  transformed <- captcha_transform_image(file)$unsqueeze(2)
  ind <- mm$model(transformed) |>
    torch::torch_argmax(3) |>
    as.matrix()
  apply(ind, 1, \(x) paste(captcha_ds$vocab[x], collapse = ""))
}

