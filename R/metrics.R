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
    tgt <- torch::torch_argmax(target$squeeze(), dim = 3)
    # browser()
    new_correct <- (pred == tgt)$to(dtype = torch::torch_float())$sum()$item()
    self$correct <- self$correct + new_correct
    self$total <- self$total + pred$numel()
  },
  compute = function() {
    self$correct / self$total
  }
)
