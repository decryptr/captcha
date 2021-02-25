#' @title Classify captchas with their answers
#'
#' @description Given one or more captcha files, this function
#' prompts you to break them mannually so that later you can train
#' a model with those answers. Answered captchas are saved at `path`
#' with their answers in the filename separated by an underscore.
#'
#' @param files A vector with the paths to captcha files
#' @param answers Either `NULL` (for interactive classification) or
#' a vector with answers for the captchas
#' @param path Where to save the renamed (answered) captcha files
#' (if `NULL`, will save each file on the same folder as its unanswered
#' counterpart)
#' @param rm_old Whether or not to delete unanswered captchas after
#' copying and renaming them
#' @param ... Other arguments passed on to [read_captcha()]
#'
#' @return A vector with the paths to the newly created files
#'
#' @export
classify <- function(files, answers = NULL, path = NULL, rm_old = FALSE, ...) {

  if ("captcha" %in% class(files)) {
    files <- files$path
  }

  # Create directory if necessary
  if (!is.null(path)) {
    fs::dir_create(path)
  }

  if (!is.null(answers)) {

    # Stop if answers don't match captchas
    stopifnot(length(answers) == length(files))

    # Iterate over each captcha
    files <- purrr::map2_chr(
      files, answers,
      classify_,
      path = path,
      rm_old = rm_old,
      ...
    )

  } else {

    # Prompt for each captcha
    files <- purrr::map_chr(
      files,
      classify_,
      ans = NULL,
      path = path,
      rm_old = rm_old,
      ...
    )
  }

  return(files)
}

classify_ <- function(cap, ans, path, rm_old, ...) {

  # Read captcha
  cap_ <- read_captcha(cap)

  # If interactive, prompt for answer
  if (is.null(ans)) {

    # TODO If passed a model, use it
    if (!is.null(list(...)$model)) {
      ans <- decrypt(cap, list(...)$model)
    } else {
      plot.captcha(cap_)
      ans <- readline("Answer: ")
    }
  }

  # Get information about where the file should be saved
  name <- tools::file_path_sans_ext(basename(cap))
  ext <- tools::file_ext(basename(cap))
  path <- ifelse(is.null(path), dirname(cap), normalizePath(path))

  # Build name of new file
  new_file <- stringr::str_glue("{path}/{name}_{ans}.{ext}")

  # Copy file to new address
  file.copy(cap, new_file, overwrite = TRUE)
  if (rm_old) {
    file.remove(cap)
  }

  return(new_file)
}
