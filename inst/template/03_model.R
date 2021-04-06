library(magrittr)

# EDIT HERE
parm <- list(
  input_dim = c(32L, 192L),
  output_vocab_size = 10L,
  vocab = 0:9,
  output_ndigits = 4L,
  path_files = "img",
  n_train = 3800L,
  batch_size = 32L,
  n_epochs = 15L
)

# data prep ---------------------------------------------------------------

files <- fs::dir_ls(parm$path_files)
# set.seed(101)
i_train <- sort(sample(seq_along(files), parm$n_train))
f_train <- files[i_train]
f_test <- files[-i_train]

# datasets
train_ds <- captcha::captcha_ds_in_memory(
  files = f_train,
  dims = parm$input_dim,
  output_dim = parm$output_vocab_size,
  transform = captcha::train_transforms
)
test_ds <- captcha::captcha_ds_in_memory(
  files = f_test,
  dims = parm$input_dim,
  output_dim = parm$output_vocab_size,
  transform = captcha::valid_transforms
)

# data loaders
train_dl <- torch::dataloader(
  dataset = train_ds,
  batch_size = parm$batch_size,
  shuffle = TRUE
)
test_dl <- torch::dataloader(
  dataset = test_ds,
  batch_size = parm$batch_size,
  drop_last = TRUE
)

model <- captcha::net_captcha(parm)$to(device = "cuda")
optimizer <- torch::optim_adam(model$parameters, lr = 0.001)

for (epoch in seq_len(parm$n_epochs)) {

  p <- progress::progress_bar$new(total = length(train_dl))

  # train step
  train_metrics <- list(total = 0, correct = 0)
  model$train()
  coro::loop(for (b in train_dl) {
    p$tick()
    res_train <- captcha::train_step(b, model, optimizer)
    train_metrics <- captcha::update_train_metrics(train_metrics, res_train)
  })

  # test step
  valid_metrics <- list(loss = c(), total = 0, correct = 0)
  model$eval()
  coro::loop(for (b in test_dl) {
    res_valid <- captcha::valid_step(b, model)
    valid_metrics <- captcha::update_valid_metrics(valid_metrics, res_valid)
  })

  captcha::print_results(epoch, train_metrics, valid_metrics)
}

# export fitted model -------------------------------------------------------

torch::torch_save(model, "model.pt")

