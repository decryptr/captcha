#' @title Annotate captchas with their labels
#'
#' @description Given one or more Captchas, this function
#' prompts the user to solve them mannually to train a model.
#' Annotated captchas are saved at `path`
#' with their labels in the filename separated by an underscore.
#'
#' @param files Either an object of class `captcha` or a character vector
#'   with the paths to captcha files
#' @param labels Either `NULL` (for interactive classification) or
#'   a character vector with labels for the Captchas. See details.
#' @param path Where to save the annotated captcha files.
#'   If `NULL`, saves the files in the same folder the unanswered counterparts.
#' @param rm_old Whether or not to delete unanswered captchas after
#' copying and renaming them.
#'
#' @details
#' The `labels=`
#' parameter can handle situations where one knows the Captcha label.
#' For example, a workflow that uses an oracle might provide the
#' label automatically. When the label doesn't exist,
#' the `captcha_annotate()` function opens the prompt for classification
#' and shows the image using `plot()`.
#'
#' @return A vector with the paths of the modified files.
#'
#' @export
captcha_annotate <- function(files,
                             labels = NULL,
                             path = NULL,
                             rm_old = FALSE) {

  if ("captcha" %in% class(files)) {
    files <- files$path
  }

  # Create directory if necessary
  if (!is.null(path)) {
    fs::dir_create(path)
  }

  if (!is.null(labels)) {

    # Stop if labels don't match captchas
    stopifnot(length(labels) == length(files))

    # Iterate over each captcha
    files <- purrr::map2_chr(
      files, labels,
      captcha_annotate_one,
      path = path,
      rm_old = rm_old
    )

  } else {

    # Prompt for each captcha
    files <- purrr::map_chr(
      files,
      captcha_annotate_one,
      lab = NULL,
      path = path,
      rm_old = rm_old
    )
  }

  return(files)
}

captcha_annotate_one <- function(cap, lab, path, rm_old) {

  # Read captcha
  cap_ <- read_captcha(cap)

  # If interactive, prompt for label
  if (is.null(lab)) {
    plot.captcha(cap_)
    lab <- readline("Label: ")
  }

  # Get information about where the file should be saved
  name <- tools::file_path_sans_ext(basename(cap))
  ext <- tools::file_ext(basename(cap))
  path <- ifelse(is.null(path), dirname(cap), normalizePath(path))

  # Build name of new file
  new_file <- stringr::str_glue("{path}/{name}_{lab}.{ext}")

  # Copy file to new address
  file.copy(cap, new_file, overwrite = TRUE)
  if (rm_old) {
    file.remove(cap)
  }

  return(new_file)
}
