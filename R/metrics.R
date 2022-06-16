#' Captcha accuracy metric
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
