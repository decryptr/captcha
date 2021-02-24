# Write the script to download captcha files

download_captcha <- function(n = 1, path = "img", ext = NULL) {

  if (is.null(ext)) ext <- ".png"
  purrr::map_chr(seq_len(n), ~download_captcha_one(path, ext))

}

download_captcha_one <- function(path, ext) {

  # captcha url goes here
  u_captcha <- ""
  f_captcha <- fs::file_temp("captcha", path, ext)

  # change method if necessary
  httr::GET(
    u_captcha,
    httr::write_disk(f_captcha, TRUE)
  )

  f_captcha
}
