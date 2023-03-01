#' Captcha accuracy metric
#'
#' This object is used to calculate the accuracy of the model in the
#' fitting process. This object is used internally inside a `luz` workflow.
#'
#' This function is a generator created using [luz::luz_metric()] function.
#' It has a `initialize()` method that sets the total number of instances
#' and total number of correct predictions as zero. For any mini batch, it
#' has an `update()` method that updates the total number of instances and
#' total number of correct predictions with new data. Finally, it has a
#' `compute()` method that calculates accuracy from the total number of
#' correct predictions and total number of instances.
#'
#' @export
captcha_accuracy <- luz::luz_metric(
  abbrev = "Captcha Acc",
  initialize = function() {
    self$correct <- 0
    self$total <- 0
  },
  update = function(preds, target) {
    # browser()
    pred <- torch::torch_argmax(preds, dim = 3)
    tgt <- torch::torch_argmax(target, dim = 3)
    # browser()

    new_correct <- torch::torch_sum(pred == tgt, 2) == dim(pred)[2]
    new_correct <- new_correct$to(dtype = torch::torch_float())$sum()$item()
    self$correct <- self$correct + new_correct
    self$total <- self$total + dim(pred)[1]
  },
  compute = function() {
    self$correct / self$total
  }
)
