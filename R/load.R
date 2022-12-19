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
#' @param captcha file name or captcha name
#' @param repo repo in the form `"<user>/<captcha>"`. Default `"decryptr/captcha"`
#' @param tag tag name of the release to load the file.
#'
#' @export
captcha_load_model <- function(captcha, repo = "decryptr/captcha", tag = "captcha_model") {
  captcha_url <- available_models()[captcha]

  if (file.exists(captcha)) {
    path <- captcha
  } else {
    f_model <- paste0(captcha, ".pt")
    dir_tmp <- tempdir("model")
    piggyback::pb_download(
      file = f_model,
      dest = dir_tmp,
      repo = repo,
      tag = tag
    )
    path <- paste0(dir_tmp, "/", f_model)
  }

  luz::luz_load(path)

}
