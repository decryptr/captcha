#' Available models for prediction using [decrypt()]()
#'
#' @export
available_models <- function() {

  c(
    "tjpe" = "https://storage.googleapis.com/decryptr/models/tjpe_93.pt"
  )

}

#' Load captcha model
#'
#' @param captcha file name or captcha name from [available_models()]()
#'
#' @export
captcha_load_model <- function(captcha) {
  captcha_url <- available_models()[captcha]

  if (is.na(captcha_url)) {
    path <- captcha
  } else {
    tmp <- fs::file_temp("model", ext = "pt")
    download.file(captcha_url, tmp)
    path <- tmp
  }

  luz::luz_load(path)
}
