valid_transforms <- function(img) {
  img %>%
    torchvision::transform_normalize(mean = .5, std = .2)
}


calcular_x <- function(x, dims) {
  x %>%
    purrr::map(torchvision::base_loader) %>%
    purrr::map(torchvision::transform_to_tensor) %>%
    purrr::map(torchvision::transform_rgb_to_grayscale) %>%
    torch:::torch_stack() %>%
    torchvision::transform_resize(dims)
}

#' Funcao de resolver o captcha
#'
#' @param file arquivo para ler
#' @param mm modelo
#' @param vocab vocabulario (ou letras + numeros de vazio),
#' @param dims dimensoes do pre processamento
#'
#' @export
decrypt <- function(file, mm, vocab, dims = c(32L, 192L)) {

  if (missing(vocab))
    vocab <- c(letters, 0:9)

  parm <- list(
    input_dim = c(32L, 192L),
    output_vocab_size = 36L,
    output_ndigits = 6L,
    path_files = "img/rfb",
    n_train = 10000L,
    batch_size = 32L,
    n_epochs = 15L
  )

  mm$to(device = "cpu")
  ans <- calcular_x(file, dims)$unsqueeze(1) %>%
    valid_transforms() %>%
    mm() %>%
    torch::torch_max(dim = 3) %>%
    purrr::pluck(2)

  paste(
    vocab[as.numeric(ans$to(device = "cpu"))],
    collapse = ""
  )

}
