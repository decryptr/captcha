#' Generate captcha
#'
#' Generates random captcha image
#'
#' @param write_disk write image to disk? Defaults to `FALSE`.
#' @param path path to save images. Defaults to current directory.
#' @param chars which chars to generate. Defaults upper/lower
#'   case letters and numbers
#' @param n_chars captcha length. Defaults to 4.
#' @param n_rows,n_cols image dimensions. Defaults to 60x120 image.
#' @param p_rotate probability to add rotation. Defaults to 80%.
#' @param p_line probability to add strikethrough line. Defaults to 80%.
#' @param p_stroke probability to add stroke color. Defaults to 30%.
#' @param p_box probability to add bounding box to text. Defaults to 30%.
#' @param p_implode probability to add imploding effect. Defaults to 20%.
#' @param p_oilpaint probability to add oilpaint effect. Defaults to 0.
#' @param p_noise probability to add random noise to image. Defaults to 40%.
#' @param p_lat probability to add LAT algorithm to image. Defaults to 0.
#'
#' @return list containing two elements: imagemagick object and captcha
#'   value.
#'
#' @examples
#'
#' captcha_generate()
#' captcha_generate(n_chars = 5)
#'
#' @export
captcha_generate <- function(write_disk = FALSE,
                             path = getwd(),
                             chars = c(0:9, letters, LETTERS),
                             n_chars = 4,
                             n_rows = 60,
                             n_cols = 120,
                             p_rotate = .8,
                             p_line = .8,
                             p_stroke = .3,
                             p_box = .3,
                             p_implode = .2,
                             p_oilpaint = 0,
                             p_noise = .4,
                             p_lat = 0) {


  gravity <- c(
    "Center"#,
    # "East",
    # "NorthEast",
    # "North",
    # "NorthWest",
    # "SouthEast",
    # "South",
    # "SouthWest",
    # "West"
  )
  fonts <- c(
    "sans", "mono", "serif", "Times", "Helvetica",
    "Trebuchet", "Georgia", "Palatino", "Comic Sans"
  )

  size <- ceiling(n_rows * n_cols / 200)

  captcha_chars <- sample(chars, n_chars, replace = TRUE)
  captcha_value <- paste(captcha_chars, collapse = "")

  rand <- stats::runif(n_rows * n_cols * 3, min = 0, max = .3)
  background_cols <- grDevices::col2rgb(sample(grDevices::colors(), 1)) / 255
  background_pix <- rep(background_cols, each = n_rows * n_cols)
  m <- array(background_pix, dim = c(n_rows, n_cols, 3))
  m <- m + rand
  m_bg <- magick::image_read(m)

  # text color can't be too close to background color
  txt_col <- sample(grDevices::colors(), 1)
  txt_col_rgb <- grDevices::col2rgb(txt_col) / 255
  dist_col <- sum((txt_col_rgb - background_cols)^2)
  while (dist_col < .3) {
    txt_col <- sample(grDevices::colors(), 1)
    txt_col_rgb <- grDevices::col2rgb(txt_col) / 255
    dist_col <- sum((txt_col_rgb - background_cols)^2)
  }

  if (stats::runif(1) < p_box) {
    # box color can't be too close to text color
    box_color <- sample(grDevices::colors(), 1)
    box_color_rgb <- grDevices::col2rgb(box_color) / 255
    dist_box_col <- sum((txt_col_rgb - box_color_rgb)^2)
    while (dist_box_col < .3) {
      box_color <- sample(grDevices::colors(), 1)
      box_color_rgb <- grDevices::col2rgb(box_color) / 255
      dist_box_col <- sum((txt_col_rgb - box_color_rgb)^2)
    }
  } else {
    box_color <- "none"
  }

  m_text <- magick::image_annotate(
    magick::image_blank(n_cols * 5, n_rows * 5),
    text = captcha_value,
    size = sample(seq(size - 2, size + 2), 1),
    gravity = sample(gravity, 1),
    color = txt_col,
    degrees = ifelse(stats::runif(1) < p_rotate, sample(seq(-10, 10), 1), 0),
    weight = sample(seq(400, 800), 1),
    kerning = sample(seq(-2, 10), 1),
    font = sample(fonts, 1),
    style = sample(magick::style_types(), 1),
    decoration = ifelse(stats::runif(1) < p_line, "LineThrough", "None"),
    strokecolor = ifelse(
      stats::runif(1) < p_stroke,
      sample(grDevices::colors(), 1),
      "none"
    ),
    boxcolor = box_color
  ) |>
    magick::image_trim() |>
    magick::image_resize(stringr::str_glue("{n_cols}x{n_rows}"))



  if (stats::runif(1) < p_implode) {
    m_text <- magick::image_implode(m_text, factor = stats::runif(1, 0, .4))
  }
  if (stats::runif(1) < p_oilpaint) {
    m_text <- magick::image_oilpaint(m_text, radius = 1.5)
  }
  if (stats::runif(1) < p_noise) {
    ## too much noise
    # ntype <- sample(setdiff(magick::noise_types(), "Random"), 1)
    m_text <- magick::image_noise(m_text, "Gaussian")
  }
  if (stats::runif(1) < p_lat) {
    lat_geo <- paste0(
      sample(seq(0,10), 1), "x",
      sample(seq(0,10), 1), "+",
      sample(seq(0,10), 1), "%"
    )
    m_text <- magick::image_lat(m_text, geometry = lat_geo)
  }


  m_complete <- magick::image_composite(
    m_bg, m_text,
    operator = "Atop",
    gravity = "center"
  )

  result <- list(
    image = m_complete,
    captcha = captcha_value
  )

  if (write_disk) {
    dir.create(path, FALSE, TRUE)
    f_captcha <- fs::file_temp(
      tmp_dir = path,
      ext = ".png",
      pattern = "captcha"
    )
    magick::image_write(m_complete, f_captcha)
    f_classify <- classify(f_captcha, tolower(captcha_value), rm_old = TRUE)
    result$file <- f_classify
  }
  result
}
