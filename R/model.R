calcular_y <- function(x, dim) {
  all_letters <- x |>
    basename() |>
    stringr::str_extract("(?<=_)[0-9a-zA-Z]+") |>
    purrr::map(stringr::str_split, "")

  vocab <- sort(unique(unlist(all_letters)))

  all_letters |>
    purrr::map(~{
      torch::torch_tensor(as.integer(factor(.x[[1]], levels = vocab)))
    }) |>
    purrr::map(torch::nnf_one_hot, dim) |>
    torch::torch_stack()
}

calcular_x <- function(x, dims) {
  x |>
    purrr::map(torchvision::base_loader) |>
    purrr::map(torchvision::transform_to_tensor) |>
    purrr::map(torchvision::transform_rgb_to_grayscale) |>
    torch::torch_stack()
}

#' Prepare image
#'
#' @name prepare
#' @param img image
#'
#' @export
train_transforms <- function(img) {
  img |>
    # torchvision::transform_random_resized_crop(
    #   c(32L, 192L), scale = c(1, 1), ratio = c(1, 1),
    #   interpolation = 0L
    # ) |>
    torchvision::transform_color_jitter() |>
    torchvision::transform_normalize(mean = .5, std = .2)
}

#' Prepare image
#'
#' @rdname prepare
#'
#' @export
valid_transforms <- function(img) {
  img |>
    torchvision::transform_normalize(mean = .5, std = .2)
}

#' @rdname prepare
#'
#' @param files captcha files
#' @param dims vector of length two with file dimentions
#' @param output_dim output (y) dimensions
#' @param transform transform function
#'
#' @export
captcha_ds_in_memory <- torch::dataset(
  name = "ds",
  initialize = function(files, dims, output_dim, transform = identity) {
    self$transform <- transform
    self$x <- calcular_x(files, dims)
    self$y <- calcular_y(files, output_dim)
    self$path = files
  },
  .getitem = function(index) {
    force(index)
    list(
      x = self$transform(self$x[index,..,drop=FALSE]),
      y = self$y[index,..],
      path = self$path[index]
    )
  },
  .length = function() {
    nrow(self$y)
  }
)

calc_dim_conv <- function(x) {
  purrr::reduce(1:3, calc_dim_img_one, .init = x)
}

calc_dim_img_one <- function(x, y) {
  floor((x-2)/2)
}


# model definition --------------------------------------------------------

#' Net captcha
#'
#' @param input_dim (integer, integer): image input dimensions.
#' @param output_ndigits number of tokens for each Captcha.
#' @param output_vocab_size number of unique token values.
#' @param vocab token labels
#' @param transform input transform function (for prediction purposes)
#' @param dropout (float, float) AlexNet dropout values.
#' @param dense_units Number of dense units
#'
#' @export
net_captcha <- torch::nn_module(

  "CAPTCHA-CNN",

  initialize = function(input_dim,
                        output_ndigits,
                        output_vocab_size,
                        vocab,
                        transform,
                        dropout = c(.25, .25),
                        dense_units = 400) {

    # in_channels, out_channels, kernel_size, stride = 1, padding = 0
    self$batchnorm0 <- torch::nn_batch_norm2d(3)
    self$conv1 <- torch::nn_conv2d(3, 32, 3)
    self$batchnorm1 <- torch::nn_batch_norm2d(32)
    self$conv2 <- torch::nn_conv2d(32, 64, 3)
    self$batchnorm2 <- torch::nn_batch_norm2d(64)
    self$conv3 <- torch::nn_conv2d(64, 64, 3)
    self$batchnorm3 <- torch::nn_batch_norm2d(64)
    self$dropout1 <- torch::nn_dropout2d(dropout[1])
    self$dropout2 <- torch::nn_dropout2d(dropout[2])

    self$fc1 <- torch::nn_linear(
      # must be the same as last convnet
      in_features = prod(calc_dim_conv(input_dim)) * 64,
      out_features = dense_units
    )
    self$batchnorm_dense <- torch::nn_batch_norm1d(dense_units)
    self$fc2 <- torch::nn_linear(
      in_features = dense_units,
      out_features = output_vocab_size * output_ndigits
    )
    self$output_vocab_size <- output_vocab_size
    self$input_dim <- input_dim
    self$output_ndigits <- output_ndigits
    self$vocab <- vocab
    self$transform <- transform
  },

  forward = function(x) {

    # x <- captcha_dl_train$.iter()$.next()$x
    # browser()
    out <- x |>
      # normalize
      self$batchnorm0() |>
      # layer 1
      self$conv1() |>
      torch::nnf_relu() |>
      torch::nnf_max_pool2d(2) |>
      self$batchnorm1() |>

      # layer 2
      self$conv2() |>
      torch::nnf_relu() |>
      torch::nnf_max_pool2d(2) |>
      self$batchnorm2() |>

      # layer 3
      self$conv3() |>
      torch::nnf_relu() |>
      torch::nnf_max_pool2d(2) |>
      self$batchnorm3() |>

      # dense
      torch::torch_flatten(start_dim = 2) |>
      self$dropout1() |>
      self$fc1() |>
      torch::nnf_relu() |>
      self$batchnorm_dense() |>
      self$dropout2() |>
      self$fc2()

    out$view(c(
      dim(out)[1],
      self$output_ndigits,
      self$output_vocab_size
    ))

  }
)

# model fitting -----------------------------------------------------------

#' Model fitting
#'
#' Models related to model fitting
#'
#' @param b batch
#' @param model model
#' @param optimizer optimizer
#' @param device "cuda" or "cpu
#'
#' @name fit
#'
#' @export
train_step <- function(b, model, optimizer, device = "cuda") {

  optimizer$zero_grad()
  # apply forward pass and compare with labels
  output <- model(b$x$to(device = device))
  labels <- b$y$to(device = device)
  loss <- torch::nnf_multilabel_soft_margin_loss(output, labels)
  # calculate error
  predicted <- torch::torch_max(output$data(), dim = 3)[[2]]
  label_values <- torch::torch_max(labels, dim = 3)[[2]]
  compare <- predicted == label_values
  # apply backward pass and update weights
  loss$backward()
  optimizer$step()

  list(
    total = label_values$size(1),
    correct = sum(apply(compare$to(device = "cpu"), 1, all))
  )

}

#' @rdname fit
#'
#' @export
valid_step <- function(b, model, device = "cuda") {

  output <- model(b$x$to(device = device))
  labels <- b$y$to(device = device)
  loss <- torch::nnf_multilabel_soft_margin_loss(output, labels)
  label_values <- torch::torch_max(labels, dim = 3)[[2]]
  predicted <- torch::torch_max(output$data(), dim = 3)[[2]]
  compare <- predicted == label_values

  list(
    loss = loss,
    total = label_values$size(1),
    correct = sum(apply(compare$to(device = "cpu"), 1, all))
  )

}

#' @rdname fit
#'
#' @param metrics metrics
#' @param res results
#'
#' @export
update_train_metrics <- function(metrics, res) {
  metrics$total <- metrics$total + res$total
  metrics$correct <- metrics$correct + res$correct
  metrics
}

#' @rdname fit
#'
#' @export
update_valid_metrics <- function(metrics, res) {
  metrics$loss <- c(metrics$loss, res$loss$item())
  metrics$total <- metrics$total + res$total
  metrics$correct <- metrics$correct + res$correct
  metrics
}

#' @rdname fit
#'
#' @param epoch epoch to print
#' @param train_metrics train metrics to print
#' @param valid_metrics valid metrics to print
#'
#' @export
print_results <- function(epoch, train_metrics, valid_metrics) {
  cat(sprintf("Loss at epoch %d: %3f\n", epoch, mean(valid_metrics$loss)))
  cat(sprintf(
    "Accuracy (train) at epoch %d: %3f\n",
    epoch,
    train_metrics$correct / train_metrics$total
  ))
  cat(sprintf(
    "Accuracy (validation) at epoch %d: %3f\n",
    epoch,
    valid_metrics$correct / valid_metrics$total
  ))
  cat("\n")
}
