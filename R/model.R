calc_dim_conv <- function(x) {
  purrr::reduce(1:3, calc_dim_img_one, .init = x)
}

calc_dim_img_one <- function(x, y) {
  floor((x-2)/2)
}

# model definition --------------------------------------------------------

#' Net captcha
#'
#' This is a torch module with This is a simple CNN with three convolutional
#' layers and two dense layers. It works well with the default dimensions
#' 32x192 from the [captcha_transform_image()] function. It also uses batch
#' normalization in the forward method. This function can be used either
#' to fit a Captcha model using the `luz` workflow suggested in
#' `vignette("advanced")` or as a base code to develop custom models.
#'
#' @param input_dim (integer, integer): image input dimensions.
#' @param output_ndigits number of tokens for each Captcha.
#' @param output_vocab_size number of unique token values.
#' @param vocab token labels
#' @param transform input transform function (for prediction purposes)
#' @param dropout (float, float) AlexNet dropout values.
#' @param dense_units Number of dense units
#'
#' @return object of classes `CAPTCHA-CNN` and `nn_module`. It works
#'   as a predictive function and as the input to a luz fitting workflow.
#'
#' @examples
#'
#' # raw image
#' captcha_file <- system.file(
#'   "examples/captcha/tjmg.jpeg",
#'   package = "captcha"
#' )
#'
#' # initializes model. The output_ndigits, output_vocab_size and vocab
#' # parameters are compatible to the TJMG Captcha.
#' model <- net_captcha(
#'   input_dim = c(32, 192),
#'   output_ndigits = 5,
#'   output_vocab_size = 10,
#'   vocab = 0:9,
#'   transform = captcha_transform_image,
#'   dropout = c(.25, .25),
#'   dense_units = 400
#' )
#' model
#'
#' transformed <- model$transform(captcha_file)
#'
#' # tensor of size 1 x output_ndigits x output_vocab_size
#' prediction <- model(transformed)
#' dim(prediction)
#'
#' # get the predicted labels
#' # the model is awful, because it is not fitted yet.
#' indices <- as.numeric(torch::torch_argmax(prediction, 3))
#' label <- paste(model$vocab[indices], collapse = "")
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
    self$dropout1 <- torch::nn_dropout(dropout[1])
    self$dropout2 <- torch::nn_dropout(dropout[2])

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
