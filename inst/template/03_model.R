train_transforms <- function(img) {
  img %>%
    # torchvision::transform_random_resized_crop(
    #   c(32L, 192L), scale = c(1, 1), ratio = c(1, 1),
    #   interpolation = 0L
    # ) %>%
    torchvision::transform_color_jitter() %>%
    torchvision::transform_normalize(mean = .5, std = .2)
}

valid_transforms <- function(img) {
  img %>%
    torchvision::transform_normalize(mean = .5, std = .2)
}

meu_dataset <- torch::dataset(
  name = "ds",
  initialize = function(files, transform = identity) {
    self$transform <- transform
    self$x <- calcular_x(files)
    self$y <- calcular_y(files)
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


files <- fs::dir_ls("dados/img/captcha_trt")
set.seed(101)
n_train <- 400
i_train <- sort(sample(seq_along(files), n_train))

f_train <- files[i_train]
f_test <- files[-i_train]

train_ds <- meu_dataset(f_train, transform = train_transforms)
test_ds <- meu_dataset(f_test, transform = valid_transforms)


# model -------------------------------------------------------------------


net_captcha <- nn_module(

  "CAPTCHA-CNN",

  initialize = function() {
    # in_channels, out_channels, kernel_size, stride = 1, padding = 0
    self$conv1 <- nn_conv2d(1, 32, 3)
    self$conv2 <- nn_conv2d(32, 64, 3)
    self$conv3 <- nn_conv2d(64, 64, 3)
    self$dropout1 <- nn_dropout2d(0.25)
    self$dropout2 <- nn_dropout2d(0.5)
    self$fc1 <- nn_linear(2816, 400)
    self$fc2 <- nn_linear(400, 54)
  },

  forward = function(x) {

    # x <- train_dl$.iter()$.next()$x
    out <- x %>%
      # layer 1
      self$conv1() %>%
      nnf_relu() %>%
      nnf_max_pool2d(2) %>%

      # layer 2
      self$conv2() %>%
      nnf_relu() %>%
      nnf_max_pool2d(2) %>%

      # layer 3
      self$conv3() %>%
      nnf_relu() %>%
      nnf_max_pool2d(2) %>%

      # dense
      self$dropout1() %>%
      torch_flatten(start_dim = 2) %>%
      self$fc1() %>%
      nnf_relu() %>%
      self$dropout2() %>%
      self$fc2()

    out$view(c(dim(out)[1], 6, 9))

  }
)

model <- net_captcha()$to(device = "cuda")

# data loaders
train_dl <- dataloader(train_ds, batch_size = 32, shuffle = TRUE)
test_dl <- dataloader(test_ds, batch_size = length(f_test), drop_last = TRUE)
optimizer <- torch::optim_adam(model$parameters, lr = 0.001)

coro::loop(for (epoch in seq_len(15)) {

  total_train <- 0
  correct_train <- 0
  model$train()
  for (b in enumerate(train_dl)) {
    optimizer$zero_grad()

    output <- model(b$x$to(device = "cuda"))
    labels <- b$y$to(device = "cuda")

    loss <- nnf_multilabel_soft_margin_loss(output, labels)

    predicted <- torch_max(output$data(), dim = 3)[[2]]
    label_values <- torch_max(labels, dim = 3)[[2]]

    total_train <- total_train + label_values$size(1)

    compare <- predicted == label_values
    correct_train <- correct_train + sum(apply(compare$to(device = "cpu"), 1, all))

    loss$backward()
    optimizer$step()
  }

  loss_valid <- c()
  total_valid <- 0
  correct_valid <- 0

  model$eval()
  for (b in enumerate(test_dl)) {
    output <- model(b$x$to(device = "cuda"))
    labels <- b$y$to(device = "cuda")
    loss <- nnf_multilabel_soft_margin_loss(output, labels)
    label_values <- torch_max(labels, dim = 3)[[2]]
    loss_valid <- c(loss_valid, loss$item())
    predicted <- torch_max(output$data(), dim = 3)[[2]]
    total_valid <- total_valid + label_values$size(1)
    compare <- predicted == label_values
    correct_valid <- correct_valid + sum(apply(compare$to(device = "cpu"), 1, all))

  }

  cat(sprintf("Loss at epoch %d: %3f\n", epoch, mean(loss_valid)))
  cat(sprintf("Accuracy (train) at epoch %d: %3f\n", epoch, correct_train / total_train))
  cat(sprintf("Accuracy (validation) at epoch %d: %3f\n", epoch, correct_valid / total_valid))
  cat("\n")
})


# export fitted model -------------------------------------------------------

torch::torch_save(model, "model.")
