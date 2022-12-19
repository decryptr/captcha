# parameter handling ------------------------------------------------------

# Edit parameters here
parm <- list(
  # Captcha parameters
  captcha_name = "my_captcha",  # Captcha name
  input_dim = c(32L, 192L),     # Resize Captcha images. Usually we don't change this
  output_vocab_size = 10L,      # Size of the vocabulary
  vocab = 0:9,                  # Character vector with the vocabulary
  output_ndigits = 4L,          # Length of the Captcha images

  # Data parameters
  path_img = "img",             # Path of all images (training and validation)
  n_train = 1000L,              # How many images to consider for training
  batch_size = 40L,             # Minibatch size

  # Model parameters
  n_epochs = 100L,              # Number of epochs
  dense_units = 200L,           # Number of dense units to use after convolution steps.
  dropout = c(0.25, 0.25),      # Dropout hyperparameter applied after convolution steps.
  decay = 0.99,                 # Weight decay applied each epoch
  learning_rate = .01,          # Learning rate

  # Control parameters
  early_stop_min_delta = .01,   # Minimum change to reset early stopping
  early_stop_patience = 20,     # Number of epochs to check early stopping
  path_log = "model_log.csv",   # Path to save csv logs
  path_training = "f_train.txt" # Save list of files used for training
)

# data prep ---------------------------------------------------------------

f_captcha <- fs::dir_ls(parm$path_img)

# Here we select the files used for training and validation.
# We choose them randomly. For reproducibility, one can set the pseudo-random
# seed using set.seet()

# set.seed(101)
ids <- seq_along(captcha_ds)
id_train <- sample(ids, parm$n_train)
id_valid <- setdiff(ids, id_train)

# save training files for reproducibility
f_train <- f_captcha[id_train]
readr::write_lines(f_train, parm$path_training)

# Dataset and dataloader --------------------------------------------------

# datasets
captcha_ds <- captcha::captcha_dataset(
  root = parm$path_img,
  captcha = NULL,
  download = FALSE
)

# dataloaders (training and validation)
captcha_dl_train <- torch::dataloader(
  dataset = torch::dataset_subset(captcha_ds, id_train),
  batch_size = parm$batch_size,
  shuffle = TRUE
)

captcha_dl_valid <- torch::dataloader(
  dataset = torch::dataset_subset(captcha_ds, id_valid),
  batch_size = parm$batch_size
)

# model -------------------------------------------------------------------

# The net_captcha module contains the model defined inside the {captcha} package.
# One can create custom models from this description
# Code here: https://github.com/decryptr/captcha/blob/master/R/model.R#L91

model <- captcha::net_captcha

# {luz} workflow ----------------------------------------------------------

fitted <- model |>
  # Set loss, optimizer and metrics
  luz::setup(
    loss = torch::nn_multilabel_soft_margin_loss(),
    optimizer = torch::optim_adam,
    metrics = list(captcha::captcha_accuracy())
  ) |>
  # Set hyperparameters
  luz::set_hparams(
    input_dim = dim(captcha_ds$data)[c(3,4)],
    output_vocab_size = dim(captcha_ds$target)[3],
    output_ndigits = dim(captcha_ds$target)[2],
    vocab = captcha_ds$vocab,
    transform = captcha_ds$transform,
    dropout = parm$dropout,
    dense_units = parm$dense_units
  ) |>
  # Set optimizer hyperparameters
  luz::set_opt_hparams(lr = parm$learning_rate) |>
  # Set dataloaders and callbacks
  luz::fit(
    captcha_dl_train,
    valid_data = captcha_dl_valid,
    epochs = parm$n_epochs,
    callbacks = list(
      luz::luz_callback_lr_scheduler(
        torch::lr_multiplicative,
        lr_lambda = function(x) parm$decay
      ),
      luz::luz_callback_early_stopping(
        "valid_captcha acc",
        min_delta = parm$early_stop_min_delta,
        patience = parm$early_stop_patience,
        mode = "max"
      ),
      luz::luz_callback_csv_logger(parm$path_log)
    )
  )


# export fitted model -------------------------------------------------------

fitted_model_path <- paste0(parm$captcha_name, ".pt")
luz::luz_save(fitted, fitted_model_path)
