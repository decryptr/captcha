#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

library(magrittr)

# paths -------------------------------------------------------------------

path_data <- "/home/jtrecenti/Documents/captchaDownload/data-raw/trt/img_oracle/"
# path_data <- "/var/tmp/jtrecenti/img_oracle"
path_log <- "data-raw/trt.log"
# path_log <- args[2]

path_model <- fs::path_ext_set(path_log, ".pt")

# download and create dataset ---------------------------------------------

captcha_ds_train <- captcha::captcha_dataset(
  root = path_data,
  captcha = NULL,
  download = FALSE
)

captcha_ds_valid <- captcha::captcha_dataset(
  root = path_data,
  captcha = NULL,
  download = FALSE
)


# create train and validation data loaders --------------------------------
set.seed(1)
ids <- seq_along(captcha_ds_train)
id_train <- sample(ids, .9 * length(captcha_ds_train))
id_valid <- setdiff(ids, id_train)
length(id_train)

captcha_dl_train <- torch::dataloader(
  torch::dataset_subset(captcha_ds_train, id_train),
  batch_size = 40,
  shuffle = TRUE
)

captcha_dl_valid <- torch::dataloader(
  torch::dataset_subset(captcha_ds_valid, id_valid),
  batch_size = 40
)

# specify model -----------------------------------------------------------

model <- captcha::net_captcha

# run model ---------------------------------------------------------------

fitted <- model |>
  luz::setup(
    loss = torch::nn_multilabel_soft_margin_loss(),
    optimizer = torch::optim_adam,
    metrics = list(captcha::captcha_accuracy())
  ) |>
  luz::set_hparams(
    input_dim = dim(captcha_ds_train$data)[c(3,4)],
    output_vocab_size = dim(captcha_ds_train$target)[3],
    output_ndigits = dim(captcha_ds_train$target)[2],
    vocab = captcha_ds_train$vocab,
    transform = captcha_ds_train$transform,
    dropout = c(0.3, 0.3),
    dense_units = 200
  ) |>
  luz::set_opt_hparams(
    lr = .01
  ) |>
  luz::fit(
    captcha_dl_train,
    valid_data = captcha_dl_valid,
    epochs = 100,
    # weight decay
    callbacks = list(
      luz::luz_callback_lr_scheduler(
        torch::lr_multiplicative,
        lr_lambda = function(x) .98
      ),
      luz::luz_callback_csv_logger(path_log)
    )
  )

luz::luz_save(fitted, "data-raw/trt_100.pt")

# captcha::decrypt(
#   "~/Documents/jtrecenti/captchaDownload/data-raw/trt/img_oracle/trt383813c7ea47_rd2kdb_1.jpeg",
#   fitted
# )

