#' Fit Captcha model
#'
#' Provides a basic interface for fitting custom models from a fully labeled
#' data. Annotation can be done manually using the [captcha_annotate()]
#' function presented earlier or with another method developed by the user.
#' The model uses a convolutional neural network architecture, similar
#' to the LeNet-5 model.
#'
#' @param dir directory where the classified images are
#' @param dir_valid (optional) directory to validation files
#' @param prop_valid proportion of total images considered to validation.
#'   Defaults to 0.2.
#' @param dropout dropout hyperparameter. Default 0.25.
#' @param dense_units number of dense units to use after convolution steps.
#'   Defaults to 200.
#' @param decay Weight decay applied each epoch.
#' @param batch_size Minibatch size. Default 40.
#' @param epochs Number of epochs to use. Default 100. The model uses early
#'   stopping, so it is possible that the procedure ends before the total
#'   number of epochs actually run.
#'
#' @return fitted model of class `luz_module_fitted`.
#'
#' The modeling step has some assumptions about the file names.
#' Images must be in a folder and have the pattern
#' `path/to/file/<id>_<lab>.<ext>`, where:
#' * `<id>`: can be any name, preferably without accents or other
#'   special characters, to avoid encoding issues. It usually contains a
#'   name for the type and a hash to identify the image uniquely.
#'   __Note__: When annotating a file, the id must be unique, as two
#'   Captchas can have the same label.
#' * `<lab>`: is the Captcha label. It is a string of characters between
#'   `[a-zA-Z0-9]`, which can be case-sensitive if necessary.
#'   All labels must have the same length.
#' * `<ext>`: file extension. It can be `.png`, `.jpeg` or `.jpg`.
#'   The operations also work for the `.svg` format, but it may have
#'   problems due to the image's transparency.
#'
#' An important note is that the model stops fitting after 20 iterations
#' without significant increment of accuracy (chosen as 1%; for more
#' details, see `vignette("advanced")`.
#'
#' @export
captcha_fit_model <- function(dir,
                              dir_valid = NULL,
                              prop_valid = 0.2,
                              dropout = 0.25,
                              dense_units = 200,
                              decay = 0.99,
                              batch_size = 40,
                              epochs = 100) {

  captcha_ds <- captcha_dataset(dir)

  if (is.null(dir_valid)) {

    ids <- seq_along(captcha_ds)
    id_train <- sample(ids, (1 - prop_valid) * length(captcha_ds))
    id_valid <- setdiff(ids, id_train)

    captcha_dl_train <- torch::dataloader(
      torch::dataset_subset(captcha_ds, id_train),
      batch_size = batch_size,
      shuffle = TRUE
    )

    captcha_dl_valid <- torch::dataloader(
      torch::dataset_subset(captcha_ds, id_valid),
      batch_size = batch_size
    )

  } else {
    captcha_dl_train <- torch::dataloader(
      captcha_ds,
      batch_size = batch_size,
      shuffle = TRUE
    )

    captcha_ds_valid <- captcha_dataset(dir_valid)

    captcha_dl_valid <- torch::dataloader(
      captcha_ds_valid,
      batch_size = batch_size
    )
  }

  net_captcha |>
    luz::setup(
      loss = torch::nn_multilabel_soft_margin_loss(),
      optimizer = torch::optim_adam,
      metrics = list(captcha_accuracy())
    ) |>
    luz::set_hparams(
      input_dim = dim(captcha_ds$data)[c(3,4)],
      output_vocab_size = dim(captcha_ds$target)[3],
      output_ndigits = dim(captcha_ds$target)[2],
      vocab = captcha_ds$vocab,
      transform = captcha_ds$transform,
      dropout = c(dropout, dropout),
      dense_units = dense_units
    ) |>
    luz::set_opt_hparams(lr = .01) |>
    luz::fit(
      captcha_dl_train,
      valid_data = captcha_dl_valid,
      epochs = epochs,
      callbacks = list(
        luz::luz_callback_lr_scheduler(
          torch::lr_multiplicative,
          lr_lambda = function(x) decay
        ),
        luz::luz_callback_early_stopping(
          "valid_captcha acc",
          min_delta = .01,
          patience = 20,
          mode = "max"
        )
      )
    )

}
